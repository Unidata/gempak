#include <mel_bufr.h>           /* BUFR library include file */
#include "geminc.h"
#include "gemprm.h"
#include "proto_uka.h"
#include "bfrcmn.h"

#define  NUMMT  200


int main ( void )
/************************************************************************
 * vg2uka            							*
 *                                                                      *
 * This program reads  a VG file and converts the elements 		*
 * from the VG file to an ASCII file. 					* 
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC      	05/04	Created					*
 * M. Li/SAIC		06/04	Added check for VG file & LABEL		*
 * M. Li/SAIC		08/04 	Added process for MCLOUD		*
 * M. Li/SAIC		09/04	Handled FXYTBL list elements		*
 * D. Kidwell/NCEP	 2/05   Ignored fields after first for CENTER   *
 * M. Li/SAIC		12/05	Removed unnecessary error message	*
 * L. Hinson/AWC         3/12   Removed hardset of grpid to 0 for       *
 *                              Radiation Hazard                        *
 ***********************************************************************/
{
    int		wrtflg, level, grpid, ier, ier1, ier2, ier3, iret;
    int         maxgnm, ii, jj;
    int         pagflg;
    long	size;
    char	fname[256], dattim[40], path[256], grp[4];
    char        alias[256], templ[256], filnam[256], sggrp[12];
    char        gtstr[12], grpch, cc[50];
    char    	fxytbl[80], center[80], fhour[80], ukafil[80], vgfile[256];
    char	**tblarr, ftbl[6][20], chlvl[4], **ctrarr;
    Boolean     readflg, done, inlist, process, mix;
    int		nitem, try, idone, numtbl, nn, len, itmlen = 20;
    int		numctr, nctr = 3;

    bfrtbl_t    ptrtbl[NUMMT];

    FILE	*fptr, *ifpout;
/*---------------------------------------------------------------------*/

    iret   = 0;
    level  = 0;
    wrtflg = 2;
    fptr   = NULL;
    ifpout = NULL;
    ctrarr = NULL;
    readflg = False;

    strcpy ( cc, " " );
    strcpy ( grp, "CVG" );
    strcpy ( sggrp, "VG2UKA");

    ip_init ( &try, &ier );
    ip_idnt ( sggrp, &ier, strlen(sggrp) );

    /*
     *  Read the master BUFR FXY table.
     */
    bfr_rdmt ( ptrtbl, &nitem, &ier );

    done = False;
    while ( !done ) {
	v2uinp ( dattim, fhour, fxytbl, center, vgfile, ukafil, &ier,
		 sizeof(dattim), sizeof(fhour), sizeof(fxytbl),
		 sizeof(center), sizeof(vgfile), sizeof(ukafil) );	

	process = True;
	/*
	 * Check for the GEMPAK date/time string.
	 */
	if ( strlen(dattim) == (size_t)0 ) {
	    pagflg = G_FALSE;
            ip_help ( sggrp, &pagflg, &ier, strlen(sggrp) );
            ier1 = -1;
	    process = False;
            er_lmsg ( &level, sggrp, &ier1, cc, &ier,
                      strlen(sggrp), strlen(cc) );
        }

        /*
         * Check for chart type/alias.
         */
	cst_rxbl ( fxytbl, fxytbl, &len, &ier );
	tblarr = (char **) malloc ( nitem * sizeof(char *) );
        for( ii = 0; ii < nitem; ii++ )
            tblarr[ii] = (char *) malloc ( itmlen * sizeof(char) );
		
        if ( strlen(fxytbl) == (size_t)0  ) {
            ier1 = -6; 
	    process = False;
            er_lmsg ( &level, sggrp, &ier1, cc, &ier,
                      strlen(sggrp), strlen(cc) );
        }
     	else {
	    cst_clst( fxytbl, ';', "", nitem, itmlen, tblarr,
                      &numtbl, &ier );

	   /*
	    * If an alias is entered, get the list of FXY tble names
	    * from master table.
	    */
	    nn = 0;
	    mix = False;
	    if ( numtbl == 1 && len == 3 ) {
	        cst_lcuc ( fxytbl, chlvl, &ier );
	    	for (ii = 0; ii < nitem; ii++) {
                    if ( strcmp(chlvl, ptrtbl[ii].alias) == 0 ) {
                    	strcpy ( ftbl[nn], ptrtbl[ii].fxyfils );
                    	nn++;
                    }
                }
                numtbl = nn;
	    }
	    else {
		for ( ii = 0; ii < nitem; ii++ ) {
		    if ( strcmp ( tblarr[0],  ptrtbl[ii].fxyfils ) == 0 ) {
			strcpy ( chlvl, ptrtbl[ii].alias );	
			break;
		    }		    
		}

		for ( ii = 0; ii < nitem; ii++ ) {
                    for ( jj = 1; jj < numtbl; jj++ ) {
                        if ( strcmp ( tblarr[jj],  ptrtbl[ii].fxyfils ) == 0 &&
                             strcmp ( chlvl, ptrtbl[ii].alias ) != 0 ) {
                            mix = True;
                            break;
                        }
                    }
                }


	       /*
	        * If the list contain a mix of high and mid level FXY file
		* names, write an error message and only process the high
		* level file names (SWH).
		*/
		if ( mix ) {
		    ier1 = -5;
            	    er_lmsg ( &level, sggrp, &ier1, cc, &ier,
                              strlen(sggrp), strlen(cc) );
 		   
		    strcpy ( chlvl, "SWH" ); 
		    for ( ii = 0;  ii < nitem; ii++ ) {
			for ( jj = 0; jj < numtbl; jj++ ) {
			    if ( strcmp( chlvl, ptrtbl[ii].alias) == 0 &&
				 strcmp ( tblarr[jj],  ptrtbl[ii].fxyfils ) == 0 ) {
				strcpy ( ftbl[nn], tblarr[jj] );
				nn++;
			    }	
			}
		    }
		    numtbl = nn;
                }
		else {
		    for ( ii = 0; ii < numtbl; ii++ ) {
			strcpy ( ftbl[ii], tblarr[ii] );
		    }
		}
	    } 
  	}

        if ( process ) {

 	   /*
 	    * Construct a VGF file name.
 	    */
 	    if ( strcmp (fhour, "18" ) != 0 ) strcpy (fhour, "24");
 	    strcpy ( alias, "SIGVGF" );
 	    strcat ( alias, fhour );

 	    ctb_dtrd ( &ier );
 	    ctb_dtpath ( alias, path, &ier );
 	    if ( ier != 0 ) {
 		ier1 = -3;
 		er_lmsg ( &level, sggrp, &ier1, fhour, &ier,
 		      strlen(sggrp), strlen(fhour) );
 	    }

 	    if ( strlen(vgfile) == (size_t)0 ) { 
 		strcpy ( filnam, path);
 		strcat ( filnam, "/");
 		ctb_dttmpl ( alias, templ, &ier );
 		cfl_mnam ( dattim, templ, fname, &ier );
 		strcat ( filnam, fname );
 	    }
 	    else {
 		strcpy ( filnam, vgfile );
 	    }

 	    /*
 	     * If VG file is not found, notify user.
 	     */
 	    cfl_inqr ( filnam, NULL, &size, path, &ier );
 	    if ( ier != 0 ) {
 		ier1 = -4;
 		er_lmsg ( &level, sggrp, &ier1, filnam, &ier,
 			  strlen(sggrp), strlen(filnam) );
		process = False;
 	    }
	}
	
	if ( process ) {
 	    /*
 	     * Open input file.
 	     */
 	    cvg_open ( filnam, wrtflg, &fptr, &ier2 );

 	    if ( ier2 == 0 ) {

 		/*
 		 * Open output file and write header information to it.
 		 */

 		if ( strlen(center) ==  (size_t)0 ) {
		    strcpy ( center, "7" );
		}
		else {
		    cst_rxbl ( center, center, &len, &ier );
	            ctrarr = (char **) malloc ( nctr * sizeof(char *) );
                    for( ii = 0; ii < nctr; ii++ )
                        ctrarr[ii] = (char *) malloc ( itmlen * sizeof(char) );
	            cst_clst( center, '/', "", nctr, itmlen, ctrarr,
                              &numctr, &ier );
 		    if ( strlen(ctrarr[0]) != (size_t)0 ) {
		        strcpy ( center, ctrarr[0] );
		    }
		    else {
		        strcpy ( center, "7" );
		    }
		}

 		if ( strlen(ukafil) == (size_t)0 ) {
 		    strcpy ( ukafil, chlvl);      
 		    strcat ( ukafil, ".txt");
 		}

 		ifpout = cas_open ( ukafil, readflg, &ier );
 		cas_wrmhdr ( ifpout, dattim, fhour, chlvl, center, &ier );

 	       /*
 		* Initialize the group type table.
 		*/
 		ces_gtrtbl ( &iret );
		
		for ( jj = 0; jj < numtbl; jj++ ) {
     	
     		   /*
     		    * Start group type searches. 
     		    *
     		    * Look for group type "CLOUD". This includes scalloped areas
     		    * and an information box.
     		    */

     		    strcpy ( gtstr, "CLOUD");
     		    inlist = False;
     		    for ( ii = 0; ii < nitem; ii++ ) {
     			if ( (strcmp ( chlvl, ptrtbl[ii].alias ) == 0) && 
 	    		     (strcmp ( gtstr, ptrtbl[ii].vgftype ) == 0) &&
			     (strcmp ( ftbl[jj],  ptrtbl[ii].fxyfils ) == 0) ) {
     			    inlist = True;
     			    break;
     			}
     		    }
     		    
     		    if ( inlist ) {	
     			ces_gtgid (gtstr, &grpid, &ier);
     			grpch = (char)grpid;
     			maxgnm = 0;

     			cvg_gtgnum ( filnam, fptr, grpch, size, &maxgnm, &iret );

     			rewind (fptr);
     			if ( ( ier == 0 ) && ( iret == 0 ) && ( maxgnm > 0 ) ) {

     			    uka_ptcld ( ifpout, filnam, fptr, size, (char)grpid, gtstr, 
     					maxgnm, &iret );
     			    if ( iret == 0 ) {
     				printf("Finished decoding CLOUD elements.\n");
     			    }
     			}
     			else {
     			    ier1 = -2;
     			    er_lmsg ( &level, sggrp, &ier1, gtstr, &ier,
     				      strlen(sggrp), strlen(gtstr) );
     			}
     		    }

     		   /*
     		    * Look for group type "TURB". This includes dashed areas 
     		    * and an information box.
     		    */
     		    strcpy ( gtstr, "TURB");
     		    inlist = False;
     		    for ( ii = 0; ii < nitem; ii++ ) {
     			if ( (strcmp ( chlvl, ptrtbl[ii].alias ) == 0) && 
     			     (strcmp ( gtstr, ptrtbl[ii].vgftype ) == 0) &&
                             (strcmp ( ftbl[jj],  ptrtbl[ii].fxyfils ) == 0) ) { 
     			    inlist = True;
     			    break;
    			}
    		    }
    		     
    		    if ( inlist ) {
    			ces_gtgid (gtstr, &grpid, &ier);
    			grpch = (char)grpid;
    			maxgnm = 0;

    			cvg_gtgnum ( filnam, fptr, grpch, size, &maxgnm, &iret);
 
    			rewind (fptr);
    			if ( ( ier == 0 ) && ( iret == 0 ) && ( maxgnm > 0 ) ) {
    			    uka_pttur ( ifpout, filnam, fptr, size, (char)grpid, gtstr, 
    					maxgnm, chlvl, &iret );
    			    if ( iret == 0 ) {
    				printf("Finished decoding TURBULENCE elements.\n");
    			    }
    			}
    			else {
    			    ier1 = -2;
    			    er_lmsg ( &level, sggrp, &ier1, gtstr, &ier,
    				      strlen(sggrp), strlen(gtstr) );
    			}
    		    }

    		   /*
    		    * Look for group type "JETS". This includes filled arrow 
    		    * lines, wind barbs, hash marks and information boxes.
    		    */
    		    strcpy ( gtstr, "JETS");
    		    inlist = False;
    		    for ( ii = 0; ii < nitem; ii++ ) {
    			if ( (strcmp ( chlvl, ptrtbl[ii].alias ) == 0) && 
    			     (strcmp ( gtstr, ptrtbl[ii].vgftype ) == 0) &&
                             (strcmp ( ftbl[jj],  ptrtbl[ii].fxyfils ) == 0) ) { 
    			    inlist = True;
    			    break;
    			}
    		    }
    		     
    		    if ( inlist ) {
    			ces_gtgid (gtstr, &grpid, &ier);
    			grpch = (char)grpid;
    			maxgnm = 0;
 
    			cvg_gtgnum ( filnam, fptr, grpch, size, &maxgnm, &iret );
 
    			/*
    			 * Look for jet elements even if no jet group were found.
    			 */
    
    			 rewind (fptr);
    			 if ( ( ier == 0 ) && ( maxgnm >= 0 ) ) {
 			 uka_ptjet ( ifpout, filnam, fptr, size, (char)grpid, gtstr, 
 				    maxgnm, chlvl, &iret );
 			 if ( iret == 0 ) {
 			     printf("Finished decoding JETS elements.\n");
 			 }
 		     }
 		     else {
 			 ier1 = -2;
 			 er_lmsg ( &level, sggrp, &ier1, gtstr, &ier,
 				    strlen(sggrp), strlen(gtstr) );
 		     }
     		    }
  
     		   /*
     		    * Look for group type "FRONT". This includes cold, warm,
     		    * occlusions, quasi-stationary fronts and any wind arrows
     		    * and speed text information.
     		    */

     		    strcpy ( gtstr, "FRONT");
     		    inlist = False;
     		    for ( ii = 0; ii < nitem; ii++ ) {
     			if ( (strcmp ( chlvl, ptrtbl[ii].alias ) == 0) && 
     			     (strcmp ( gtstr, ptrtbl[ii].vgftype ) == 0) &&
                             (strcmp ( ftbl[jj],  ptrtbl[ii].fxyfils ) == 0) ) { 
     			    inlist = True;
     			    break;
     			}
     		    }
  
     		    if ( inlist ) {
     			ces_gtgid (gtstr, &grpid, &ier);
     			grpch = (char)grpid;
     			maxgnm = 0;

     			cvg_gtgnum ( filnam, fptr, grpch, size, &maxgnm, &ier1 );

     			rewind (fptr);
     			if ( ( ier == 0 ) && ( ier1 == 0 ) && ( maxgnm > 0 ) ) {
     			    uka_ptfrt ( ifpout, filnam, fptr, size, (char)grpid, gtstr,
     					maxgnm, chlvl, &iret );
     			    if ( iret == 0 ) {
     				printf("Finished decoding FRONT elements.\n");
     			    }
     			}
     			else {
     			    ier1 = -2;
     			    er_lmsg ( &level, sggrp, &ier1, gtstr, &ier,
     					strlen(sggrp), strlen(gtstr) );
     			}
     		    }
  
     		   /*
     		    * Look for the ungrouped tropopause boxes. These are the border
     		    * filled, high and low special text boxes.
     		    */
  
     		    strcpy ( gtstr, "TROP");
     		    inlist = False;
     		    for ( ii = 0; ii < nitem; ii++ ) {
     			if ( (strcmp ( chlvl, ptrtbl[ii].alias ) == 0) && 
     			     (strcmp ( gtstr, ptrtbl[ii].vgftype ) == 0) &&
                             (strcmp ( ftbl[jj],  ptrtbl[ii].fxyfils ) == 0) ) { 
     			    inlist = True;
     			    break;
 		    	}
 		    }

 		    if ( inlist ) {
 		        grpid = '0';

 		        rewind (fptr);
 		        uka_pttrp ( ifpout, filnam, fptr, size, (char)grpid, gtstr, chlvl, &iret );
 		        if ( iret == 0 ) {
 			    printf("Finished decoding TROPOPAUSE elements.\n");
 		        }
 		    }

 	           /*
 		    * Look for group type "LABEL". This includes volcanoes,
 		    * tropical storms and radiaion symbols and associated text.
 		    */

 		    strcpy ( gtstr, "LABEL");
 		    inlist = False;
 		    for ( ii = 0; ii < nitem; ii++ ) {
 		        if ( (strcmp ( chlvl, ptrtbl[ii].alias ) == 0) && 
                             (strcmp ( gtstr, ptrtbl[ii].vgftype ) == 0) &&
                             (strcmp ( ftbl[jj],  ptrtbl[ii].fxyfils ) == 0) ) { 
 			    inlist = True;
 			    break;
 		        }
 		    }

 		    if ( inlist ) {
 		        ces_gtgid (gtstr, &grpid, &ier);
 		        grpch = (char)grpid;
 		        maxgnm = 0;

 		        rewind (fptr);
 		        cvg_gtgnum ( filnam, fptr, grpch, size, &maxgnm, &ier1 );

 		        if ( ( ier == 0 ) && ( ier1 == 0 ) && ( maxgnm >= 0 ) ) {
 			    uka_ptstm ( ifpout, filnam, fptr, size, (char)grpid, gtstr,
 			    	    maxgnm, &iret );
			    ier3 = iret;

 			    rewind (fptr);
 		 	    uka_ptvol  ( ifpout, filnam, fptr, size, (char)grpid, gtstr,
 				      maxgnm, &iret );
			    ier3 += iret;
                            
 			    rewind (fptr);
 			    uka_ptrad ( ifpout, filnam, fptr, size, (char)grpid, gtstr, &iret );
			    ier3 += iret;

			    if ( ier3 != 48 ) {
 			        printf("Finished decoding SYMBOLS.\n");
			    }
 		        }
 		        else {
 			    ier1 = -2;
 			    er_lmsg ( &level, sggrp, &ier1, gtstr, &ier,
 				  strlen(sggrp), strlen(gtstr) );
 		        }
 		    }

                   /*
                    * Look for group type "MCLOUD". This includes non-Cb type, 
		    * non-Cb distribution, turbulence, icing and Cb information.
                    */

                    strcpy ( gtstr, "MCLOUD");
                    inlist = False;
                    for ( ii = 0; ii < nitem; ii++ ) {
                        if ( (strcmp ( chlvl, ptrtbl[ii].alias ) == 0) &&
                             (strcmp ( gtstr, ptrtbl[ii].vgftype ) == 0) &&
                             (strcmp ( ftbl[jj],  ptrtbl[ii].fxyfils ) == 0) ) {
                            inlist = True;
			    strcpy ( gtstr, "CLOUD");
                            break;
                        }
                    }

                    if ( inlist ) {
                        ces_gtgid (gtstr, &grpid, &ier);
                        grpch = (char)grpid;
                        maxgnm = 0;

                        cvg_gtgnum ( filnam, fptr, grpch, size, &maxgnm, &iret );

                        rewind (fptr);
                        if ( ( ier == 0 ) && ( iret == 0 ) && ( maxgnm > 0 ) ) {

                            uka_ptmcld ( ifpout, filnam, fptr, size, (char)grpid, gtstr,
                                         maxgnm, &iret );
                            if ( iret == 0 ) {
                                printf("Finished decoding MCLOUD elements.\n");
                            }
                        }
                        else {
                            ier1 = -2;
                            er_lmsg ( &level, sggrp, &ier1, gtstr, &ier,
                                      strlen(sggrp), strlen(gtstr) );
                        }
		    }
		}

   	    }
 	    else {
 	        er_lmsg ( &level, grp, &ier2, filnam, &ier,
 	                  strlen(grp), strlen(filnam) );
 	   }

 	   /*
 	    *  Close VG and output files.
 	    */

 	    if ( fptr != NULL ) cvg_clos ( fptr, &iret );
 	    if ( ifpout != NULL ) cas_clos ( ifpout, &ier );

       } /* if ( process )  */

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
    
    for ( ii=0; ii < nitem; ii++ ) {
        if ( tblarr[ii] ) {
            free( tblarr[ii] );
        }
    }
    free( tblarr );

    if ( ctrarr ) {
        for ( ii=0; ii < nctr; ii++ ) {
            if ( ctrarr[ii] ) {
                free( ctrarr[ii] );
            }
        }
        free( ctrarr );
    }
	
    return (0);
}
