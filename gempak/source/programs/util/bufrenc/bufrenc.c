#include <mel_bufr.h>
#include "geminc.h"
#include "gemprm.h"
#include "bfrcmn.h"

#define	 NUMMT	200
#define	 NUMAREA 	4

int main ( void )
/************************************************************************
 * bufrenc                                                              *
 *                                                                      *
 * This program reads ASCII files and encodes the information into BUFR *
 * format.                                                              *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		10/03	Created					*
 * M. Li/SAIC	 	11/03	Modified NUMMT & fvalue			*
 * M. Li/SAIC		12/03	Changed template value for output files	* 
 * M. Li/SAIC		01/04	Changed the places of initialization	*
 * M. Li/SAIC		02/04	Free allocated memory			*
 * M. Li/SAIC		02/04	Reposition memory free			*
 * M. Li/SAIC		10/04	Processed SWM				*
 * M. Li/SAIC		10/04	Added BUFR section2			*
 * M. Li/SAIC		11/04	Rearranged memory allocation		* 
 ***********************************************************************/
{
    int	    ii, jj, len, lenf, nn, ipos, numerr, leverr, ier, ier1;
    char    **tblarr, **filarr, timestr[10];
    char    fxytbl[80], bufrfil[80], fhour[10], infile[80], datatype[30];
    char    errgrp[8], cc[50];
    int	    itmlen = 20, outlen = 20, fxy_i[NUMMT], fxy_vals[NUMMT]; 
    int	    nfxy, numtbl, numfil, ifhour, nitem, maxfxy;
    int	    ih[] = {0, 6, 12, 18, 24, 30, 36, 42, 48, 60, 72};
    char    *fvalue[]={"A", "B", "C", "D", "E", "F", "G", "H" "I", "J", "K"};
    int	    itime[5], jtime[5];
    int	    idcent, info_bufr[8], narea ;
    char    chlvl[4], tblnam[30], dirsym[10], tag0[72], value[120];
    char    **area, swm_areas[250], section2[250], area_latln[NUMAREA][61];
    char    outstr[61];
    char    *tag[] = { "MASTER_TABLE", "EDITION", 
		       "UPDATE_SEQ_NUMBER", "DATA_CATEGORY", 
	 	       "DATA_SUBCATEGORY", "VERSION_NUMBER_MASTER", 
		       "VERSION_NUMBER_LOCAL", "OBSERVED_DATA_FLAG" };
    int	    info_def[] = {0, 3, 0, 7, 255, 11, 0, 0};
    int     numtrp, rnum, lnum, hnum, membx, memhi, memlo;
    int     memcld, memmcld, memfrt, memjet, memstm, memtur, memvlr;
    int     numcld, nummcld, numfrt, numjet, numstm, numtur, numvlr;
    float   chbase, chtop;

    cloud_t     *ptrc, *head, *ptr2;
    mcloud_t    *ptrm, *headm, *ptr2m;
    jets_t      *ptrj, *headj, *ptr2j;
    front_t     *ptrf, *headf, *ptr2f;
    turb_t      *ptrb, *headb, *ptr2b;
    storm_t     *ptrs, *heads, *ptr2s;
    volrad_t    *ptrv, *headv, *ptr2v;
    trop_t      *ptrr, *headr, *ptr2r;
    trophi_t    *ptrh, *headh, *ptr2h;
    troplo_t    *ptrl, *headl, *ptr2l;


    bfrtbl_t    ptrtbl[NUMMT];

    Boolean readflg, done, mix;
    int   	try, idone;
    FILE	*fptr;
/*---------------------------------------------------------------------*/

    leverr = 0;
    fptr   = NULL;
    tblarr = NULL;
    filarr = NULL;
    area   = NULL;
    strcpy ( errgrp, "BUFRENC" );
    strcpy ( cc, " " );
    ip_init ( &try, &ier );
    ip_idnt ( errgrp, &ier, strlen(errgrp) );

    /*
     *  Read the master BUFR FXY table.
     */
    bfr_rdmt ( ptrtbl, &nitem, &ier );

   /*
    * Get values for BUFR section0, 1, and 3.
    */

    strcpy ( tblnam, "bufrprefs.tbl" );
    strcpy ( dirsym, "bufr" );

    for ( ii = 0; ii < 8; ii++ ) {
    	ctb_rdprf ( tblnam, dirsym, tag[ii], value, &ier1 );
	if ( ier1 == -1 && ii == 0 ) {
            er_lmsg ( &leverr, "CTB", &ier1, tblnam, &ier,
                      3, strlen(tblnam) );
	    for ( jj = 0; jj < 8; jj++ ) {
		info_bufr[jj] = info_def[jj];
	    }
	    break;
        }
	else if ( ier1 == -2 ) {
	    ier1 = -6;
	    er_lmsg ( &leverr, "CTB", &ier1, tag[ii], &ier,
                      3, strlen(tag[ii]) ); 
	    info_bufr[ii] = info_def[ii];
	}
	else {
    	    sscanf ( value, "%d", &info_bufr[ii] );    
	}
    }

   /*
    * Get areas to be included in section2.
    */

    area = (char **) malloc ( NUMAREA * sizeof (char *) );
    for ( ii = 0; ii < NUMAREA; ii++ )
	area[ii] = (char *) malloc ( 60 * sizeof (char) ); 

    narea = 0;
    strcpy ( tag0, "AREAS_TO_INCLUDE" );
    ctb_rdprf ( tblnam, dirsym, tag0, value, &ier1 );

    if ( ier1 == -2 ) {
        ier1 = -6;
        er_lmsg ( &leverr, "CTB", &ier1, tag0, &ier,
                  3, strlen(tag0) );
        swm_areas[0] = '\0';
    }
    else {
	cst_clst( value, ';', "", NUMAREA, 60, area,
                      &narea, &ier );
    }

   /*
    * Generate section2 string.
    */

    if ( narea > 0 ) {
  	for ( ii = 0; ii < narea; ii++ ) {
	    ctb_rdprf ( tblnam, dirsym, area[ii], area_latln[ii], &ier1 ); 	
	    if ( ier1 == -2 ) {
		ier1 = -6;
        	er_lmsg ( &leverr, "CTB", &ier1, area[ii], &ier,
                  	  3, strlen(area[ii]) );
        	area_latln[ii][0] = '\0';
	    }

	    cst_rnan ( area_latln[ii], area_latln[ii], &ier );
	    cst_padString ( area_latln[ii], ' ', 1, 60, outstr );
   	    if ( ii == 0 ) {
		strcpy ( swm_areas, outstr );
	    }
	    else {
		strcat ( swm_areas, outstr );
	    }
	}
    }
	
    done = False;
    while ( !done ) {
	bfeinp ( fxytbl, bufrfil, fhour, infile, &ier, sizeof(fxytbl), 
		 sizeof(bufrfil), sizeof(fhour), sizeof(infile) );  

       /*
	* Check for input of fxytbl.
	*/

 	tblarr = (char **) malloc ( nitem * sizeof(char *) );

        for( ii = 0; ii < nitem; ii++ )
            tblarr[ii] = (char *) malloc ( itmlen * sizeof(char) );

	cst_rxbl ( fxytbl, fxytbl, &len, &ier );

	nn = 0;
	if ( strlen(fxytbl) > (size_t)3 ) {
            cst_clst( fxytbl, ';', "", nitem, itmlen, tblarr,
                      &numtbl, &ier );
	}
	else if ( strlen(fxytbl) == (size_t)3 ) {

           /*
            * If an alias is entered, get the list of FXY table names
            * from master table.
            */

	    cst_lcuc ( fxytbl, fxytbl, &ier );
	    if ( strcmp ( fxytbl, "SWH" ) == 0 || strcmp ( fxytbl, "SWM" ) == 0 ) {
                for (ii = 0; ii < nitem; ii++) {
                    if ( strcmp(fxytbl, ptrtbl[ii].alias) == 0 ) {
                        strcpy ( tblarr[nn], ptrtbl[ii].fxyfils );
                        nn++;
                    }
                }
                numtbl = nn;
		strcpy ( chlvl, fxytbl );
	    }
	    else {
		done = True;
	    }
	} 
	else {
	    done = True;
	}

	if ( done ) {
	    numtbl = IMISSD;
	    numerr = -1;
            er_lmsg ( &leverr, errgrp, &numerr, fxytbl, &ier,
                  strlen(errgrp), strlen(fxytbl) );
	}

       /*
        * Check for the mix of high and mid level FXY file names.
        */
	mix = False;
	if ( !done && strlen(fxytbl) > (size_t)3 && numtbl > 0 )  {
	    for ( ii = 0; ii < nitem; ii++ ) {
                if ( strcmp ( tblarr[0],  ptrtbl[ii].fxyfils ) == 0 ) {
                    strcpy ( chlvl, ptrtbl[ii].alias );
                    break;
                }
            }
	
	    if ( strcmp ( chlvl, "SWH") == 0 || strcmp ( chlvl, "SWM") == 0 ) {
                for ( ii = 0; ii < nitem; ii++ ) {
                    for ( jj = 1; jj < numtbl; jj++ ) {
                        if ( strcmp ( tblarr[jj],  ptrtbl[ii].fxyfils ) == 0 &&
                             strcmp ( chlvl, ptrtbl[ii].alias ) != 0 ) {
                            mix = True;
                            break;
                        }
                    }
                }

                if ( mix ) {
                    done = True;
                    numerr = -7;
                    er_lmsg ( &leverr, errgrp, &numerr, fxytbl, &ier,
                              strlen(errgrp), strlen(fxytbl) );
                }


	    }
	    else {
                done = True;
                numerr = -1;
                er_lmsg ( &leverr, errgrp, &numerr, fxytbl, &ier,
                  strlen(errgrp), strlen(fxytbl) );

	    }
	}
	

	if ( !done ) {

	   /*
            * Get BUFR file names.
            */

	    filarr = (char **) malloc ( nitem * sizeof(char *) );

            for( ii = 0; ii < nitem; ii++ )
                filarr[ii] = (char *) malloc ( outlen * sizeof(char) );

	    cst_rxbl ( bufrfil, bufrfil, &len, &ier );
	    if ( len > 0 ) {
                cst_clst( bufrfil, ';', "", nitem, outlen, filarr,
                          &numfil, &ier );
	    }	
	    /*
             * Get the BUFR file names from master table.
	     */
	    else if ( len == 0 ) {
 	        for (ii = 0; ii < nitem; ii++) {
		    for (jj = 0; jj < numtbl; jj++) { 
		        if ( strcmp(tblarr[jj], ptrtbl[ii].fxyfils) == 0 ) {
                	    strcpy ( filarr[jj], ptrtbl[ii].bufrout );
		        }
		    }
                }
	        numfil = numtbl;
	    }

	   /*
	    * If fhour is entered, get the value of "f".
	    */
	    jj = IMISSD;
	    cst_rxbl ( fhour, fhour, &lenf, &ier ); 
	    if ( lenf > 0 ) {
	        cst_numb ( fhour, &ifhour, &ier );
	        for ( ii = 0; ii < 11; ii++ ) { 
		    if ( ih[ii] == ifhour ) jj = ii;
	        }

	        if ( jj >= 0 ) {
	            for ( ii = 0; ii < numfil; ii++ ) {
		        if ( filarr[ii][3] == 'f' )
		    	    filarr[ii][3] = fvalue[jj][0];
	            } 
	        }
	    }

	   /*
	    * If the ASCII file is entered, open it.
	    */
	    readflg = True;
	    if ( strlen(infile) > (size_t)0 ) {
	        fptr = cas_open ( infile, readflg, &ier ); 
	        if ( ier != 0 ) {
		    done = True;
		    numerr = -3;
           	    er_lmsg ( &leverr, errgrp, &numerr, infile, &ier,
                    	      strlen(errgrp), strlen(infile) );
	        }
	    }
	    else {
	        done = True;
	        numerr = -2;
                    er_lmsg ( &leverr, errgrp, &numerr, infile, &ier,
                              strlen(errgrp), strlen(infile) );
	    }

	}

	if ( !done ) {

	    cas_rdhdr ( fptr, itime, jtime, &idcent, &chbase, &chtop, &ier );
	    if ( ier == 0 ) {
	        sprintf ( timestr, "%04d%02d%02d%02d", itime[0], itime[1], itime[2], itime[3] );
	        for ( ii = 0; ii < numfil; ii++ ) {
	     	    cst_srch ( 0, strlen(filarr[ii]), "YYYYMMDDHH", filarr[ii], &ipos, &ier );
	 	    if ( ier == 0 ) {
		        cst_rpst ( filarr[ii], "YYYYMMDDHH", timestr, filarr[ii], &ier );
		    }
	        }

	       /*
		* Check for the chart levels.
		*/

		if ( ( strcmp ( chlvl, "SWH") == 0  && 
		       ( G_ABS ( HI_BASE - chbase ) > 0.1F || G_ABS ( HI_TOP - chtop ) > 0.1F ) ) ||
		     ( strcmp ( chlvl, "SWM") == 0  && 
                       ( G_ABS ( MID_BASE - chbase ) > 0.1F || G_ABS ( MID_TOP - chtop ) > 0.1F ) ) ) {

		    done = True;
                    numerr = -8;
                    er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
                              strlen(errgrp), strlen(cc) );
		}
		     
	    }
	    else {
	        done = True;
	        numerr = -4;
                er_lmsg ( &leverr, errgrp, &numerr, infile, &ier,
                          strlen(errgrp), strlen(infile) );
	    }

	   /*
	    * If the number of BUFR output files do not match the number
	    * of FXY table names, exit the program.
	    */
	    if ( numfil != numtbl ) {
	        done = True;
	        numerr = -5;
	        er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
                          strlen(errgrp), strlen(cc) );
	    }

	}

	if ( !done ) {

   	   if ( strcmp ( chlvl, "SWH" ) == 0 ) {
	       strcpy ( section2, "" );
   	   }
	   else {
	       strcpy ( section2, swm_areas );
   	   }

	  /*
	   * Loop to call cas and bfr routines.
	   */
	  maxfxy = NUMMT; 
	  for ( ii = 0; ii < numtbl; ii++ ) {
	    strcat ( tblarr[ii], ".tbl" );
	    bfr_rdfxy ( tblarr[ii], maxfxy, datatype, &nfxy, fxy_i, fxy_vals, &ier );

	    if ( ier != 0 ) {
		done = True;
		numerr = -6;
            	er_lmsg ( &leverr, errgrp, &numerr, tblarr[ii], &ier,
                          strlen(errgrp), strlen(tblarr[ii]) );
		continue;
	    }
	    if (!fptr) continue;  

	    /*
	     * CLOUD
	     */ 
	    rewind (fptr);
	    if ( strcmp ( datatype, "CLOUD" ) == 0 ) { 
		memcld = 0;
		numcld = 0;
		ptrc   =  NULL;
		cas_rdcld ( fptr, &numcld, &ptrc, &memcld, &ier );

		bfr_cld ( filarr[ii], numcld, ptrc, itime, jtime, &nfxy, 
		          fxy_i, fxy_vals, idcent, info_bufr, section2, &ier );

		/* Free cloud */
        	if ( memcld ) {
            	    head = ptrc;
            	    ptr2 = head -> next;
            	    while ( ptr2 != NULL ) {
                	free (head);
                	head = ptr2;
                	ptr2 = ptr2 -> next;
            	    }
            	    free ( head );
        	}
	    } 

	    /*
             * FRONT
             */
	    rewind (fptr);
            if ( strcmp ( datatype, "FRONT" ) == 0 ) { 
		memfrt = 0;
		numfrt = 0;
		ptrf = NULL;
                cas_rdfrt ( fptr, &numfrt, &ptrf, &memfrt, &ier);
		bfr_frt ( filarr[ii], numfrt, ptrf, itime, jtime, &nfxy, 
		          fxy_i, fxy_vals, idcent, info_bufr, section2, &ier );
		
		/* Free front*/
        	if ( memfrt ) {
            	    headf = ptrf;
            	    ptr2f = headf -> next;
            	    while ( ptr2f != NULL ) {
                	free (headf);
                	headf = ptr2f;
                	ptr2f = ptr2f -> next;
            	    }
            	    free ( headf );
        	}
	    }

	    /*
             * JET  
             */
	    rewind (fptr);
            if ( strcmp ( datatype, "JET" ) == 0 ) { 
		memjet = 0;
		numjet = 0;
		ptrj   =  NULL;
                cas_rdjets ( fptr, &numjet, &ptrj, &memjet, &ier );
	        bfr_jet ( filarr[ii], numjet, ptrj, itime, jtime, &nfxy, 
			  fxy_i, fxy_vals, idcent, info_bufr, section2, &ier );

		/* Free jet */
        	if ( memjet ) {
            	    headj = ptrj;
            	    ptr2j = headj -> next;
            	    while ( ptr2j != NULL ) {
                	free (headj);
                	headj = ptr2j;
                	ptr2j = ptr2j -> next;
            	    }
            	    free ( headj );
        	}
	    }

	    /*
             * TURB 
             */
	    rewind (fptr);
            if ( strcmp ( datatype, "TURB" ) == 0 ) { 
		memtur = 0;
		numtur = 0;
		ptrb = NULL;
                cas_rdturb ( fptr, &numtur, &ptrb, &memtur, &ier );
	        bfr_tur ( filarr[ii], numtur, ptrb, itime, jtime, &nfxy, 
			  fxy_i, fxy_vals, idcent, info_bufr, section2, &ier );

		/* Free turb */
        	if ( memtur ) {
            	    headb = ptrb;
            	    ptr2b = headb -> next;
            	    while ( ptr2b != NULL ) {
                	free (headb);
                	headb = ptr2b;
                	ptr2b = ptr2b -> next;
            	    }
            	    free ( headb );
        	}
	    }

	    /*
             * TROP 
             */
            rewind (fptr);
            if ( strcmp ( datatype, "TROP" ) == 0 )  { 
		membx = memlo = memhi = 0;
		rnum  = lnum  = hnum  = 0;
		ptrr   =  NULL;
    		ptrh   =  NULL;
    		ptrl   =  NULL;
                cas_rdtrop ( fptr, &numtrp, &ptrr, &membx, &rnum,
                 	     &ptrl, &memlo, &lnum, &ptrh, &memhi,
                 	     &hnum, &ier );
		bfr_trp ( filarr[ii], rnum, ptrr, hnum, ptrh, lnum, ptrl, 
			  itime, jtime, &nfxy, fxy_i, fxy_vals, idcent, 
			  info_bufr, section2, &ier );

		/* Free trop */
        	if ( membx ) {
            	    headr = ptrr;
            	    ptr2r = headr -> next;
            	    while ( ptr2r != NULL ) {
                	free ( headr );
                	headr = ptr2r;
                	ptr2r = ptr2r -> next;
            	    }
            	    free ( headr );
        	}

        	if ( memhi ) {
            	    headh = ptrh;
            	    ptr2h = headh -> next;
            	    while ( ptr2h != NULL ) {
                	free ( headh );
                	headh = ptr2h;
                	ptr2h = ptr2h -> next;
            	    }
            	    free ( headh );
        	}

        	if ( memlo ) {
            	    headl = ptrl;
            	    ptr2l = headl -> next;
            	    while ( ptr2l != NULL ) {
                	free ( headl );
                	headl = ptr2l;
                	ptr2l = ptr2l -> next;
            	    }
            	    free ( headl );
		}

	    }

	    /*
             * STORM|VOLCANO|RADIATION
             */
            rewind (fptr);
            if ( strcmp ( datatype, "STORM|VOLCANO|RADIATION" ) == 0 ) { 
		memstm = 0;
		numstm = 0;
		ptrs = NULL;   
		cas_rdstm ( fptr, &numstm, &ptrs, &memstm, &ier );

	    	rewind (fptr); 
		memvlr = 0;
		numvlr = 0;
		ptrv = NULL;
	    	cas_rdvlrd ( fptr, &numvlr, &ptrv, &memvlr, &ier );


		bfr_vts ( filarr[ii], numstm, ptrs, numvlr, ptrv, itime, jtime, 
			  &nfxy, fxy_i, fxy_vals, idcent, info_bufr, section2, &ier );

		/* Free STORM|VOLCANO|RADIATION	 */	
		if ( memstm ) {
            	    heads = ptrs;
            	    ptr2s = heads -> next;
            	    while ( ptr2s != NULL ) {
                	free ( heads );
                	heads = ptr2s;
                	ptr2s = ptr2s -> next;
            	    }
            	    free ( heads );
        	}

        	if ( memvlr ) {
            	    headv = ptrv;
            	    ptr2v = headv -> next;
            	    while ( ptr2v != NULL ) {
                	free ( headv );
                	headv = ptr2v;
                	ptr2v = ptr2v -> next;
            	    }
            	    free ( headv );
        	}
	    }

            /*
             * MCLOUD
             */
            rewind (fptr);
            if ( strcmp ( datatype, "MCLOUD" ) == 0 ) {
                memmcld = 0;
                nummcld = 0;
                ptrm   =  NULL;
                cas_rdmcld ( fptr, &nummcld, &ptrm, &memmcld, &ier );
                bfr_mcld ( filarr[ii], nummcld, ptrm, itime, jtime, &nfxy, 
			   fxy_i, fxy_vals, idcent, info_bufr, section2, &ier );
                /* Free cloud */
                if ( memmcld ) {
                    headm = ptrm;
                    ptr2m = headm -> next;
                    while ( ptr2m != NULL ) {
                        free (headm);
                        headm = ptr2m;
                        ptr2m = ptr2m -> next;
                    }
                    free ( headm );
                }
            }

	  } /* End of for loop */

	}

        /*
         * Close input file.
         */
        cas_clos ( fptr, &ier );

	/*
	 * Prompt for next diagnostic to be done.
	 */
        ip_dynm ( &idone, &ier );
	if ( idone ) {
            done = True;
        }
        else {
            done = False;
        }


    } /* End of while loop */

    ip_exit ( &ier );
	

    /*
     * Free allocated memory.
     */

    if ( filarr ) {
    	for ( ii = 0; ii < nitem; ii++ ) {
	    if ( filarr[ii] ) {
                free( filarr[ii]);
	    }
        }
    free( filarr );
    }

    if ( tblarr ) {
	for ( ii = 0; ii < nitem; ii++ ) {
	    if ( tblarr[ii] ) {
            	free( tblarr[ii] );
            }
        }
    free( tblarr );
    }

    if ( area ) { 
    	for ( ii = 0; ii < NUMAREA; ii++ ) {
	    if ( area[ii] ) {
            	free( area[ii]);
	    }
        }
    free( area );
    }


    return (0);

}
