#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"

#include "proto_uka.h"
#include "cascmn.h"

#define  CLOUD_LINE 	3

void uka_ptcld ( FILE *ifpout, char *fname, FILE *fptr, long size,
                 char gptyp, char *gtstr, int maxgnm, int *iret )
/************************************************************************
 * uka_ptcld								*
 *                                                                      *
 * This function scans a VG file for elements of group type "CLOUD" and *
 * prints out all that match to an ASCII file.				*
 *                                                                      *
 * uka_ptcld ( ifpout, fname, fptr, size, gptyp, gtstr, maxgnm, iret )  *
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
 * A. Hardy/SAIC        11/01   Created					*
 * A. Hardy/SAIC         2/02   Fixed freeing memory error		*
 * A. Hardy/SAIC         2/02   Made changes for unlabeled cloud line	*
 * A. Hardy/SAIC         3/02   Added checks for bad cloud bases & tops * 
 * A. Hardy/SAIC         4/02   Added check for flipped line		*
 * A. Hardy/NCEP         6/02   Added sig_grpchk 			*
 * A. Hardy/NCEP         8/02   Added check for cloud text array num.   *
 * M. Li/SAIC		 3/04	Checked for flipped line		*
 * M. Li/SAIC		 5/04	Copied from sig_cloud			*
 * M. Li/SAIC		 9/04	Changed level from 2 to 0		*
 * L. Hinson/AWC         3/12   Fixed Codes for OCNL/OCNL EMBD/ISOL/    *
 *                              ISOL EMBD ON CBs                        *
 ***********************************************************************/
{
    int 	istoff, sbtyp, ieloff, gpnum, flag;
    int         members[MAXOFF], level; 
    int		ii, jj, ij, kk, ingrp, numclds, ier, ier1;
    int	 	*tmpgrp, numstr;
    int		tmpdis, tmptyp;
    float       top, base, tmplv1, tmplv2;
    char	vgclss, vgnum, grp[4], cget, sggrp[12], strln[256];
    char        **aryptr;
    Boolean	done, found, memalc, process;

    cloud_t	*cld=NULL, *head, *new, *ptr, *ptr2;
    VG_DBStruct     el;
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

    aryptr = (char **) malloc(sizeof(char *) * 5);
    for ( ii = 0; ii < 5; ii++ ) {
	aryptr[ii] = (char *) malloc(40);
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

                    if ( el.hdr.vg_class == CLASS_TEXT )  {

		       /* 
		        * Parse text string into cloud distribution,
		        * cloud type, cloud top and base.
		        */

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
		            if ( strcmp (aryptr[0], "OCNL") == 0 ) {
                              if ( strcmp(aryptr[1], "EMBD") == 0 ) {
		    	        tmpdis = 11;
                              } else {
                                tmpdis = 10;
                              }
		            }
                            if ( strcmp (aryptr[0], "ISOL") == 0 ) {
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
                            new = ( cloud_t *) malloc (sizeof ( cloud_t ) );
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

		        cld -> level1  = tmplv1;
		        cld -> level2  = tmplv2;
		        cld -> clddist = tmpdis;
		        cld -> cldtyp  = tmptyp;

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
    * Write out CLOUD information in BUFR ASCII format.
    */

    if ( ier == 0 ) {
        ptr = head;
        cas_wrcld ( ifpout, ptr, numclds, &ier );
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

    for ( ii = 0; ii < 5; ii++ ) {
        free ( aryptr[ii] );
    }
    free ( aryptr );
    free ( tmpgrp );

}
