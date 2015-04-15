#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"

#include "proto_uka.h"
#include "cascmn.h"

#define  CLOUD_LINE 	3

void uka_ptmcld ( FILE *ifpout, char *fname, FILE *fptr, long size,
                 char gptyp, char *gtstr, int maxgnm, int *iret )
/************************************************************************
 * uka_ptmcld								*
 *                                                                      *
 * This function scans a VG file for elements of group type "MCLOUD" for*
 * a mid level SIGWX (SWM) chart, and prints out all that match to an 	*
 * ASCII file as "MCLOUD" data.						*
 *                                                                      *
 * uka_ptmcld ( ifpout, fname, fptr, size, gptyp, gtstr, maxgnm, iret ) *
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout		FILE		Output file name pointer	*
 *      *fname		char		Input VG file name		*
 *      *fptr		FILE		Input file pointer		*
 *      size		long		Size of input file in bytes	*
 *      gptyp		char		"CLOUD" group type number	*
 *      *gtstr		char		"CLOUD" group type name		*
 *	maxgnm		int		Maximum number of group numbers *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *					= 0 normal 			*
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		 8/04	Created                 		*	
 * M. Li/SAIC		 8/04	Expanded check for cloud distribution	*
 * M. Li/SAIC		 9/04	Added a check for more turb. definition	*
 * L. Hinson/AWC         1/14   Fixed Codes for OCNL/OCNL EMBD/ISOL/    *
 *                              ISOL EMBD ON CBs                        *
 * L. Hinson/AWC         4/14   Fixed Code Checks for Severe Icing      *
 ***********************************************************************/
{
    int 	istoff, sbtyp, ieloff, gpnum, flag;
    int         members[MAXOFF], level; 
    int		ii, jj, ij, kk, ingrp, numclds, ier, ier1;
    int	 	*tmpgrp, numstr;
    int		tmpdis, tmptyp;
    float       top, base, tmplv1, tmplv2;
    char	vgclss, vgnum, grp[4], cget, sggrp[12], strln[256];
    char        **aryptr, **arycb;
    char	*typlst[9] = {"CI", "CC", "CS", "AC", "AS", "NS", "SC", 
			      "ST", "CU"};
    char	*dislst[5] = {"SKC", "SCT", "BKN", "OVC", "LYR"};
    int		dismap[] = { 0, 2, 3, 4, 14 };
    int		ilst[2], nn, type, ttype, ntyp, nctyp[9], ncld, ncdis[5];
    int		dic, nic, discb, sptxtyp, ix3, ix4;
    float	ttop, tbase, itop, ibase, cbtop, cbbase;

    Boolean	done, found, memalc, process;

    mcloud_t	*cld=NULL, *head, *new, *ptr, *ptr2;
    VG_DBStruct el;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    istoff  = 0;
    numclds = 0;
    level   = 0;
    done    = False;
    memalc  = False;
    process = False;
    strcpy(grp, "CVG");
    strcpy(sggrp, "UKA");

   /*
    * Set VG Group Class, VG Group Type number and VG Subtype number.
    */

    vgclss = CLASS_LINES;
    vgnum  = SPLN_ELM;
    sbtyp  = CLOUD_LINE;

   /*
    * Declaring the memory size for parsing the text string
    * later. Also declare the memory size for temporary group
    * number array.
    */

    aryptr = (char **) malloc(sizeof(char *) * 8);
    for ( ii = 0; ii < 8; ii++ ) {
	aryptr[ii] = (char *) malloc(40);
    }

    arycb = (char **) malloc(sizeof(char *) * 9);
    for ( ii = 0; ii < 9; ii++ ) {
        arycb[ii] = (char *) malloc(6);
    }

    tmpgrp = (int *) malloc(sizeof(int *) * (size_t)maxgnm);
    for ( ij = 0; ij < maxgnm; ij++){
	tmpgrp[ij] = 0;
    }


   /*
    * Read an element header to find the group type number.
    */

    while ( !done ) {

        cvg_rdgtn ( fname, fptr, &size, istoff, gptyp, vgclss, vgnum, 
                    sbtyp, &ieloff, &gpnum, &ier );

        istoff = ieloff;
        if ( ( gpnum > 0 ) && (ier == 0 ) ){
	    if ( tmpgrp[gpnum-1] == 0 ) {
		 found = False;
	    }
	}

       /* 
        * If the element offset exists, the group number has been found, 
        * the return value is equal to 0 and a special line has not been
	* found, continue.
        */

        if ( ( ieloff != IMISSD ) && ( gpnum > 0 ) && ( *iret == 0 ) &&
	     ( !found ) ) {

	    tmplv1 = SIGRLMS;
	    tmplv2 = SIGRLMS;
	    tmpdis = SIGIMSS;
	    tmptyp = SIGIMSS;

            cvg_srchgrp ( fname, fptr, &size, gptyp, gpnum, MAXOFF, 
	                  members, &ingrp, &ier);
            uka_grpchk ( fname, fptr, size, gtstr, members, ingrp, 
	                 &process, &ier);
            if ( ier != 0 ) {
		sprintf (strln,"CLOUD group number %d", gpnum);
                er_lmsg ( &level, sggrp, &ier, strln, &ier1,
	                  strlen(sggrp), strlen(strln) );
	    }

            if ( process ) {
            
                for ( ii = 0; ii < ingrp; ii++ ) {
	            cvg_rdhdr ( fname, fptr, members[ii], (int)size, &el, 
		                &flag, &ier );
                    cvg_rdele(&el, members[ii], el.hdr.recsz, fptr, &ier);

		   /* For regular SWH box, parse text string into cloud 
   		    * distribution, cloud type, cloud top and base.
                    */

                    if ( el.hdr.vg_class == CLASS_TEXT  && 
			 el.elem.spt.info.sptxtyp == 4 )  {

			sptxtyp = 4;
		        cget = CHLF;
		        cst_clst ( el.elem.spt.text, cget, " ", 5, 40, 
			            aryptr, &numstr, &ier );
                        if ( numstr > 3 ) {
		            if ( strcmp (aryptr[numstr-1], "XXX") != 0 ) {
		                cst_crnm ( aryptr[numstr-1], &base, &ier );
			        if ( ier == 0 ) {
				    base = base * 100.0F;
		                    tmplv1 = pr_hgfm (&base);
			        }
		            }
		            if ( strcmp (aryptr[numstr-2], "XXX") != 0 ) {
		                cst_crnm ( aryptr[numstr-2], &top, &ier );
			        if ( ier == 0 ) {
				    top = top * 100.0F;
		                    tmplv2 = pr_hgfm (&top);
			        }
		            }

		           /* 
		            * Determine cloud distribution.  
		            */
		            if ( strcmp (aryptr[0], "FRQ") == 0 ) {
			        tmpdis = 12;
		            }
		            else if ( strcmp (aryptr[0], "OCNL") == 0 ) {
                                if ( strcmp(aryptr[1], "EMBD") == 0 ) {
		    	          tmpdis = 11;
                                } else {
                                  tmpdis = 10;
                                }
		            }
                            else if ( strcmp (aryptr[0], "ISOL") == 0 ) {
                                if ( strcmp(aryptr[1], "EMBD") == 0 ) {
			          tmpdis = 9;
                                } else {
                                  tmpdis = 8;
                                }
		            }

		           /* 
		            * Determine cloud type.  
		            */

		            if ( strcmp (aryptr[numstr-3], "CB") == 0 ) {
			        tmptyp = 9;
		            }
			}
			else {
	                    tmplv1 = SIGRLMS;
	                    tmplv2 = SIGRLMS;
	                    tmpdis = SIGIMSS;
	                    tmptyp = SIGIMSS;
			}
	            }

		   /* For mid level cloud box, parse text string into
                    * types, bases/tops, and distributions of
                    * cloud, icing, turbulence and thunderstorm.
		    */

		    else if ( el.hdr.vg_class == CLASS_TEXT &&
			      el.elem.spt.info.sptxtyp == 15 )  {
			sptxtyp = 15;
                        cst_clst ( el.elem.spt.text, '|', " ", 8, 40, 
                                    aryptr, &numstr, &ier );

		       /*
			* Non-Cb cloud types.
			*/
			ntyp = 0;
			nn = 0;
			if ( strlen(aryptr[0]) > (size_t)1 ) {
			    cst_clst ( aryptr[0], ';', " ", 9, 5,
                                    arycb, &nn, &ier );

			    for ( ij = 0; ij < nn; ij++ ) {
				for (jj = 0; jj < 9; jj++ ) {
				   if ( strcmp ( arycb[ij], typlst[jj] ) == 0 ) {
					nctyp[ntyp] = jj;
					ntyp++;
					break;
				    }
				}
			    }
			} 

		       /*
			* Cloud distributions.
			*/
			ncld = 0;
			ix3 = ix4 = -1;
			if ( strlen(aryptr[1]) > (size_t)2 ) {
			    cst_clst ( aryptr[1], ';', " ", 5, 5, arycb, &nn, &ier );
                            for ( ij = 0; ij < nn; ij++ ) {
				for ( jj = 0; jj < 5; jj++ ) {
                                   if ( strcmp ( arycb[ij], dislst[jj] ) == 0 ) {
                                        ncdis[ncld] = dismap[jj];
					if ( ncdis[ncld] == 3 ) ix3 = ncld;
					if ( ncdis[ncld] == 4 ) ix4 = ncld;
                                        ncld++;
                                        break;
                                    }
                                }
                            }

			    if ( ncld == 2 && ncdis[0] == 3 && ncdis[1] == 4 ) {
				ncld = 1;
				ncdis[0] = 7;
			    }
			    if ( ncld > 2 && ix3 != -1 && ix4 != -1 ) {
				ncdis[ix3] = 7;
				if ( ix4 != (ncld - 1) ) ncdis[ix4] = ncdis[ncld-1];
				ncld -= 1;
			    }
			}

		       /*
			* Degree of icing.
			*/
			dic = SIGIMSS;
		 	cst_numb ( aryptr[2], &nic, &ier );

			if ( nic == 5 || nic == 6 ) {
			    dic = 5;
			}
			else if ( nic == 7 || nic == 8 ) {
			    dic = 8;
			}

		       /*
		       	* Top/base of icing.
			*/
			ibase = itop = SIGRLMS;
			if ( strlen (aryptr[3]) > (size_t)0 ) {
			    cst_ilst( aryptr[3], '/', SIGIMSS, 2, ilst, &nn, &ier ); 

			    if ( ilst[0] != SIGIMSS && nn > 0 ) {
			        itop  = ilst[0] * 100.0F;
				itop  = pr_hgfm ( &itop );
			    }
			
			    if ( ilst[1] != SIGIMSS && nn > 0 ) {
                                ibase = ilst[1] * 100.0F;
                                ibase = pr_hgfm ( &ibase );
                            }
			}
			    	
		       /*
			* Turbulence type, top/base.
			*/
			ttype = SIGIMSS;
			ttop = tbase = SIGRLMS;
			cst_numb ( aryptr[4], &type, &ier );

			if ( type == 4 ) {
			    ttype = 2;
			}
			else if ( type == 5 || type == 56 ) {
			    ttype = 19;
			}
		        else if ( type == 6 ) {
			    ttype = 3;
			}
			
			if ( strlen ( aryptr[5] ) > (size_t)0 ) {
                            cst_ilst( aryptr[5], '/', SIGIMSS, 2, ilst, &nn, &ier );

			    if ( ilst[0] != SIGIMSS && nn > 0 ) {
                                ttop  = ilst[0] * 100.0F;
                                ttop  = pr_hgfm ( &ttop );
                            }

                            if ( ilst[1] != SIGIMSS && nn > 0 ) {
                                tbase = ilst[1] * 100.0F;
                                tbase = pr_hgfm ( &tbase );
                            }
                        }

		       /*
			* Cb cloud distribution.
			*/ 
			discb = SIGIMSS;
			if ( strlen(aryptr[6]) > (size_t)2 ) {
			    cst_clst ( aryptr[6], ';', "", 4, 6,
                                    arycb, &nn, &ier );

			    if ( (nn == 2) && (strcmp (arycb[0], "ISOL") == 0)
				           && (strcmp (arycb[1], "EMBD") == 0) ) { 
				discb = 9;
			    }
			    else if ( (nn == 2) && (strcmp (arycb[0], "OCNL") == 0)
				                && (strcmp (arycb[1], "EMBD") == 0) ) {
                                discb = 11;
                            }
			    else {
			        if ( strcmp (arycb[0], "ISOL") == 0 ) {
				    discb = 8;
			        }
				else if ( strcmp (arycb[0], "OCNL") == 0 ) {
                                    discb = 10;
                                }
				else if ( strcmp (arycb[0], "FRQ") == 0 ) {
                                    discb = 12;
                                }
				else if ( strcmp (arycb[0], "EMBD") == 0 ) {
                                    discb = 9;
                                }
			    }
			}

		       /*
			* Cb cloud top/base.
			*/
			cbbase = cbtop = SIGRLMS;
                        if ( strlen ( aryptr[7] ) > (size_t)0 ) {
                            cst_ilst( aryptr[7], '/', SIGIMSS, 2, ilst, &nn, &ier );

			    if ( ilst[0] != SIGIMSS && nn > 0 ) {
                                cbtop  = ilst[0] * 100.0F;
                                cbtop  = pr_hgfm ( &cbtop );
                            }

                            if ( ilst[1] != SIGIMSS && nn > 0 ) {
                                cbbase = ilst[1] * 100.0F;
                                cbbase = pr_hgfm ( &cbbase );
                            }
                        }

		    }
	        }


	       /*
	        * Begin looping through the individual elements with the 
	        * same group type and group number.
	        */

                for ( ii = 0; ii < ingrp; ii++ ) {

	            cvg_rdhdr ( fname, fptr, members[ii], (int)size, &el, 
		                &flag, &ier );
                    cvg_rdele(&el, members[ii], el.hdr.recsz, fptr, &ier);

	            if ( ( el.hdr.vg_class == CLASS_LINES) &&
	                 ( el.hdr.vg_type == SPLN_ELM )    &&
	                 ( el.elem.spl.info.spltyp == CLOUD_LINE ) ) {

                       /*
                        * Check to see if the element in the group is a 
                        * cloud type line or cloud label information.
                        */

                        if ( ingrp > 0 ) {
                            new = ( mcloud_t *) malloc (sizeof ( mcloud_t ) );
			    memalc = True;
                            new->next = NULL;
                            if ( numclds != 0 ) {
	                        cld -> next = new;
                                cld = new;
                            }
                            else {
                                cld = new;
                                head = cld;
                            }
		        }

		        cld -> npt = el.elem.spl.info.numpts;
                       /*
		        * Check to see if line is flipped.
		        */

			kk = 0;
		        if ( el.elem.spl.info.spldir == 1 ) {
                            for ( jj = (cld->npt - 1); jj >= 0; jj-- ) {
                                cld->lat[jj] = el.elem.spl.latlon[kk];
                                cld->lon[jj] = el.elem.spl.latlon[kk +
                                              el.elem.spl.info.numpts];
                                kk++;
                            }

			    kk--;
		        }
		        else {
			    for ( jj = 0; jj < cld->npt;jj++ ) {
                                cld -> lat[jj] = el.elem.spl.latlon[jj];
                                cld -> lon[jj] = el.elem.spl.latlon[jj +
                                              el.elem.spl.info.numpts];
                            }
		        }

		       /*
		        * Check for a closed line flag.  If true, add
		        * the first point to the last array position.
		        */

		        if (  el.hdr.closed == 1 ) {
		            cld -> lat[cld->npt] = el.elem.spl.latlon[kk];
		            cld -> lon[cld->npt] = el.elem.spl.latlon[kk + 
			                      el.elem.spl.info.numpts];
		            cld -> npt = el.elem.spl.info.numpts+1;
		        }

		       /*
		        * Store cloud information into current linked list.
		        */

			if ( sptxtyp == 4 ) {
		            cld -> cbbase = tmplv1;
		            cld -> cbtop  = tmplv2;
		            cld -> cbdis  = tmpdis;
		            cld -> cbtyp  = tmptyp;
			    if ( !G_DIFF(tmplv1, SIGRLMS) || !G_DIFF(tmplv2, SIGRLMS) ) {
				cld -> fcb    = 1;
			    }
			    else {
				cld -> fcb    = 0;
			    }

			    cld -> ncld   = 0;
			    cld -> ntyp   = 0;
			    cld -> turb   = 0;
			    cld -> icing  = 0;
			}
			else if ( sptxtyp == 15 ) {
			    cld -> ncld   = ncld; 
			    for (jj = 0; jj < ncld; jj++ ) {
				cld -> ncdis[jj] = ncdis[jj]; 
			    }

			    cld -> ntyp = ntyp; 
			    for (jj = 0; jj < ntyp; jj++ ) {
                                cld -> nctyp[jj] = nctyp[jj];
                            }

			    cld -> tdeg   = ttype;
			    cld -> tbase  = tbase;
			    cld -> ttop   = ttop;
			    if ( ( !G_DIFF(ttop, SIGRLMS) || !G_DIFF(tbase, SIGRLMS) ) &&
				 ( ttype != SIGIMSS) ) {
				cld -> turb = 1;
			    }
			    else {
				cld -> turb = 0;
			    } 

			    cld -> dic    = dic;
			    cld -> icbase = ibase;
			    cld -> ictop  = itop;
			    if ( (!G_DIFF(ibase, SIGRLMS) || !G_DIFF(itop, SIGRLMS) ) &&
				 (dic != SIGIMSS) ) {
                                cld -> icing = 1;
                            }
                            else {
                                cld -> icing = 0;
                            }

			    cld -> cbdis  = discb;
			    cld -> cbbase = cbbase;
			    cld -> cbtop  = cbtop;
			    cld -> cbtyp  = 9;
			    if ( (!G_DIFF(cbtop, SIGRLMS) || !G_DIFF(cbbase, SIGRLMS) ) &&
				 ( discb != SIGIMSS) ) {
                                cld -> fcb = 1;
                            }
                            else {
                                cld -> fcb = 0;
                            }
		  	}

			/*
			 * Set flag for group number found.
			 */
		      	found = True;
			tmpgrp[gpnum-1] = 1;
			numclds++;
	            }
                }
	    } 
        }
	else if ( ( ieloff == IMISSD ) || ( ier != 0 ) ) {
	    ier = 0;
	    done = True; 
	    if ( numclds == 0 ) {
	        ier = 1;
                er_lmsg ( &level, sggrp, &ier, gtstr, &ier1,
	                  strlen(sggrp), strlen(gtstr) );
	    }
	}
    }

   /* 
    * Write out MCLOUD information in BUFR ASCII format.
    */

    if ( ier == 0 ) {
        ptr = head;
        cas_wrmcld ( ifpout, ptr, numclds, &ier );
    }
   /*
    * Free memory from the linked list.
    */

    if ( memalc ) {
        ptr = head;
        ptr2 = ptr -> next;
        while ( ptr2  != NULL ) {
	    free ( ptr );
            ptr = ptr2;
	    ptr2 = ptr2 -> next;
        }
            free ( ptr );
    }

   /*
    * Free memory from parsing text string and temp. group number.
    */

    for ( ii = 0; ii < 8; ii++ ) {
        free ( aryptr[ii] );
    }
    free ( aryptr );

    for ( ii = 0; ii < 9; ii++ ) {
        free ( arycb[ii] );
    }
    free ( arycb );

    free ( tmpgrp );

}
