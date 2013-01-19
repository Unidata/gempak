#include "geminc.h" 
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"

#include "proto_uka.h"
#include "cascmn.h"

#define  FRONT_QUAS	000
#define  FRONT_COLD 	200
#define  FRONT_WARM 	400
#define  FRONT_OCCL 	600

#define  ITCZ_LINE       23
#define  ITCZ_LINE_Z     26

void uka_ptfrt ( FILE *ifpout, char *fname, FILE *fptr,	long size,
	         char gptyp, char *gtstr, int maxgnm, char *chlvl, int *iret )
/************************************************************************
 * uka_ptfrt								*
 *                                                                      *
 * This function scans a VG file for elements of group type "FRONT" and *
 * prints out all that match to an ASCII file.				*
 *                                                                      *
 * uka_ptfrt ( ifpout, fname, fptr, size, gptyp, gtstr, maxgnm, chlvl,	*
 *		iret )  						*
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout		FILE		Output file name pointer	*
 *      *fname		char		Input VG file name		*
 *      *fptr		FILE		Input file pointer		*
 *      size		long		Size of input file in bytes	*
 *      gptyp		char		"FRONT" group type number	*
 *      *gtstr		char		"FRONT" group type name		*
 *	maxgnm		int		Maximum number of group numbers *
 *	*chlvl		char		Chart level			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *					= 0 normal			*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        11/01   Created					*
 * A. Hardy/SAIC         2/02   Fixed freeing memory error 		*
 * A. Hardy/SAIC         3/02   Added initialization for speed & direc. *
 * A. Hardy/SAIC         4/02   Added check for flipped line; check for *
 *                              closest speed text to diretional arrow  *
 * A. Hardy/SAIC         5/02   Added search for ITCZ line type		*
 * A. Hardy/NCEP         6/02   Added sig_grpchk			*
 * A. Hardy/NCEP         7/02   Store old group number for comparison   *
 * M. Li/SAIC		 3/04	Convert speed to m/s			*
 * M. Li/SAIC		 5/04	Copied from sig_front			*
 * M. Li/SAIC		 9/04	Added chlvl				*
 * M. Li/SAIC		12/04	Checked for slow front			*
 * S. Gilbert/NCEP       6/05   Added check for ITCZ_LINE_Z		*
 * M. Li/SAIC		11/05	Replaced cgr_* with clo_*		*
 ***********************************************************************/
{
    int 	istoff, sbtyp, ieloff, gpnum, flag;
    int         members[MAXOFF], level, istrt, iend, nout, mxpt; 
    int		ii, jj, ij, kk, ll, nn, np, npair, ingrp, numfnt, ier, ier1;
    int	 	*tmpgrp, tmpfrt, tfrtnm, tmppt, nclose;
    int         newidx[MAXPTS], itcznum;
    int         order[MAXPTS], inum, gpold;
    float       idxspd[MAXPTS], idxdir[MAXPTS], speed;
    float	tmpdir, tmpspd[12], tmplat[MAXPTS], tmplon[MAXPTS];
    float       dens, crvscl, xcv[MAXPTS], ycv[MAXPTS];
    float       plat, plon, ptlat, ptlon, pdist[12];
    double	dlta;
    char	vgclss, vgnum, grp[4], sggrp[12], strln[256];
    Boolean	done, found, memalc, skip, process, slwfrt;

    front_t	*fnt=NULL, *head=NULL, *new, *ptr, *ptr2;
    VG_DBStruct     el;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    istoff  = 0;
    numfnt  = 0;
    level   = 0;
    istrt   = 0;
    gpold   = 0;
    itcznum = 0;
    tmpfrt  = -1;
    np      = 1;
    dens    = 5.0F;
    crvscl  = 30.0F;
    dlta    = 0.001;
    mxpt    = MAXPTS;
    done    = False;
    skip    = False;
    found   = False;
    memalc  = False;
    process = False;
    strcpy(grp, "CVG");
    strcpy(sggrp, "UKA");

   /*
    * Set VG Group Class, VG Group Type number and VG Subtype number.
    */

    vgclss = CLASS_FRONTS;
    vgnum  = FRONT_ELM;
    sbtyp  = -99;

   /*
    * Declare the memory size for temporary group number array.
    */

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
        * Check if another front is found in the same group number.
	*/

	if ( gpold == gpnum ) {
	    found = True;
	}

       /* 
        * If the element offset exists, the group number has been found, 
        * the return value is equal to 0 and a special line has not been
	* found, continue.
        */

        if ( ( ieloff != IMISSD ) && ( gpnum > 0 ) && ( ier == 0 ) &&
	     ( !found ) ) {

            cvg_srchgrp ( fname, fptr, &size, gptyp, gpnum, MAXOFF, 
	                  members, &ingrp, &ier);
	    uka_grpchk ( fname, fptr, size, gtstr, members, ingrp, 
	 	         &process, &ier);

	    slwfrt = False;
            if ( ier != 0 ) {
		if ( ier == 10 ) {
		    slwfrt = True;
		    ier = 6;
		}
		sprintf (strln,"FRONT group number %d", gpnum);
                er_lmsg ( &level, sggrp, &ier, strln, &ier1,
	                  strlen(sggrp), strlen(strln) );
	    } 

	    if ( process ) {

	       /*
	        * Begin looping through the individual elements with the 
	        * same group type and group number.
	        */

	       /*
	        *  Get the front line first.
	        */

                for ( ii = 0; ii < ingrp; ii++ ) {

	            cvg_rdhdr ( fname, fptr, members[ii], (int) size, &el, 
		                &flag, &ier );
                    cvg_rdele(&el, members[ii], el.hdr.recsz, fptr, &ier);

	            if ( ( el.hdr.vg_class == CLASS_FRONTS) &&
	                 ( el.hdr.vg_type == FRONT_ELM ) ) {
		           tmpfrt = ( el.elem.frt.info.fcode / 100 );
		           tfrtnm = tmpfrt * 100;
	                if ( ( tfrtnm == FRONT_QUAS ) || 
		           ( tfrtnm == FRONT_COLD ) ||
		           ( tfrtnm == FRONT_WARM ) ||
		           ( tfrtnm == FRONT_OCCL ) ) {

                            tmppt  = el.elem.frt.info.numpts;
		            iend   = MAXPTS;

		           /*
		            * Check to see if line is flipped.
		            */

		            if ( el.elem.frt.info.fpipdr != 1 ) {
			        kk = 0;
		                for ( jj = (tmppt-1); jj >= 0; jj-- ) {
		                    tmplat[jj] = el.elem.frt.latlon[kk];
		                    tmplon[jj] = el.elem.frt.latlon[kk + 
			                      el.elem.frt.info.numpts];
	                            kk++;
			        }
		            }
		            else {
		                for ( jj = 0; jj < tmppt; jj++ ) {
		                    tmplat[jj] = el.elem.frt.latlon[jj];
		                    tmplon[jj] = el.elem.frt.latlon[jj + 
			                      el.elem.frt.info.numpts];
			        }
		            }

                           /*
		            * Find parametric curve.
		            */

		            cv_prmt ( &tmppt, tmplat, tmplon, &dens, 
		                      &mxpt, &crvscl, &istrt, &iend, &nout, 
			              xcv, ycv, &ier );

		            if ( !found ) {
			            found = True;
			            tmpgrp[gpnum-1] = 1;
		            }
	                }
                    }
		    /*
		     * Look for ITCZ line.
		     */
	            else if ( ( el.hdr.vg_class == CLASS_LINES ) &&
	                    ( el.hdr.vg_type == SPLN_ELM )     &&
		            ( el.elem.spl.info.spltyp == ITCZ_LINE ||
		              el.elem.spl.info.spltyp == ITCZ_LINE_Z ) ) {

                            tmppt  = el.elem.spl.info.numpts;
		            tmpfrt = 8;

                        for ( jj = 0; jj < tmppt; jj++ ) {
		            tmplat[jj] = el.elem.spl.latlon[jj];
		            tmplon[jj] = el.elem.spl.latlon[jj + 
			                      el.elem.spl.info.numpts];
		            idxdir[jj] = SIGRLMS;
		            idxspd[jj] = SIGRLMS;
			}
		        skip = True;
	            }
                }

	        if ( !skip ) {
                   /*
	            * Find the original unsmoothed line points in the 
	            * smoothed line and create a new index using the 
	            * smoothed line index numbers.
	            */

	            nn    = 0;
	            npair = nout;

	            for ( kk = 0; kk < tmppt; kk++) {
		        for ( ll = 0; ll < npair; ll++ ) {
		            if ( ( fabs((double)(tmplat[kk] - xcv[ll])) < dlta ) && 
		                 ( fabs((double)(tmplon[kk] - ycv[ll])) < dlta ) ) {
		                 newidx[nn] = ll;
			         nn++;
		            }
		        }
                    }
                    for ( kk = 0; kk < npair; kk++) {
                        idxspd[kk] = SIGRLMS;
                        idxdir[kk] = SIGRLMS;
	            }

                    for ( ii = 0; ii < ingrp; ii++ ) {
	                cvg_rdhdr ( fname, fptr, members[ii], (int) size, &el, 
		                &flag, &ier );
                        cvg_rdele(&el, members[ii], el.hdr.recsz, fptr, &ier);

	               /* 
		        * Check for a front directional arrow. 
		        */

		        tmpdir = SIGRLMS;
		        speed  = SIGRLMS;
                        if ( ( ( el.hdr.vg_class == CLASS_WINDS ) && 
		               ( el.hdr.vg_type == ARROW_ELM )  ) ||
			     ( ( el.hdr.vg_class == CLASS_TEXT ) && ( slwfrt ) ) ) {

			    inum = 0;
			    if ( !slwfrt ) {
		            	tmpdir = 
		                    el.elem.wnd.data.spddir[el.elem.wnd.info.numwnd]; 
                            	plat = el.elem.wnd.data.latlon[0];
		            	plon = el.elem.wnd.data.latlon[0+el.elem.wnd.info.numwnd];
			    

                               /*
	                    	*  Search for the closest text element to set 
		            	*  the speed of the front.   
	                    	*/

                            	for ( ij = 0; ij < ingrp; ij++ ) {
	                            cvg_rdhdr ( fname, fptr, members[ij], 
		                            	(int)size, &el,  &flag, &ier );
                                    cvg_rdele ( &el, members[ij], el.hdr.recsz, 
				           	fptr, &ier);

		                    tmpspd[inum] = SIGRLMS;
                                    if  ( el.hdr.vg_class == CLASS_TEXT ) {
					clo_dist ( &plat, &plon, &np, &(el.elem.spt.info.lat),
						   &(el.elem.spt.info.lon), &pdist[inum], &ier);
		                    	cst_crnm( el.elem.spt.text, &tmpspd[inum], 
				              	  &ier);
		                    	inum++;
		                    }
	                    	}
			    }
			    else {
				cst_crnm (el.elem.spt.text, &tmpspd[0], &ier);	
				if ( fabs(tmpspd[0]) < dlta ) {
	    			    plat = el.elem.spt.info.lat;
	    			    plon = el.elem.spt.info.lon;
	    			    inum = 1;
				}
				else  {
	    			    process = False;
	    			    break;
				}
			    }

		           /*
		            * If multiple speeds are found for a front determine 
		            * which text element is closest to the arrow.
		            */ 

		            if ( inum-1 > 0 ) {
		                ij = 0;
		                while ( ij < inum-1 ) {
		                    if (  pdist[ij] <  pdist[ij+1] ) {
		                        speed = tmpspd[ij];
			            }
			            else {
		                        speed = tmpspd[ij+1];
			            }
			            ij++;
		                }
		            }
		            else if ( inum-1 == 0 ) {
		                speed= tmpspd[0];
		            }

                           /*
		            * Find the arrow's closest point on the parametric 
		            * curved line.
		            */

		            nclose = 1;
	                    clo_closest ( xcv, ycv, npair, plat, plon, nclose,
		                      order, &ier ); 
		    
                           /*
	                    * Insert the closest point and associated wind speed,
		            * if there is one, into the new index array.
	                    */

		            nn = tmppt-1;
		            while ( (  nn >= 0  ) && ( newidx[nn] >= order[0] ) ) {
		                newidx[nn+1] = newidx[nn];
                                idxspd[nn+1] = idxspd[nn];
                                idxdir[nn+1] = idxdir[nn];
		                xcv[newidx[nn+1]] = xcv[newidx[nn]];
		                ycv[newidx[nn+1]] = ycv[newidx[nn]];

	                        nn--;
		            }

		            if ( nn >= 0 ) {
				ptlat = xcv[order[0]];
				ptlon = ycv[order[0]];
		            }
		           /*
		            * If arrow is at beginning of front, set to first point.
		            */
		            else {
		                ptlat = xcv[newidx[0]];
		                ptlon = ycv[newidx[0]];
		            }

		            newidx[nn+1] = order[0];
                            idxspd[nn+1] = speed;
		            idxdir[nn+1] = tmpdir;
		            xcv[newidx[nn+1]] = ptlat;
		            ycv[newidx[nn+1]] = ptlon;

		            tmppt++; 
	                }
	            }
	        }

               /*
	        * Store front information into current linked list.
	        */

                if ( ingrp > 0  && process ) {
		    numfnt++;
                    new = ( front_t *) malloc (sizeof ( front_t ) );
		    memalc = True;
                    new->next = NULL;
                    if ( numfnt != 1 ) {
	                fnt -> next = new;
                        fnt = new;
                    }
                    else {
                        fnt = new;
                        head = fnt;
                    }

	            fnt -> npt  = tmppt;
	            fnt -> ftype = tmpfrt;

	            for ( jj = 0; jj < tmppt; jj++ ) {
		        if ( !skip ) {
		            fnt -> lat[jj] = xcv[newidx[jj]];
		            fnt -> lon[jj] = ycv[newidx[jj]];
		        }
		        else {
		            fnt -> lat[jj] = tmplat[jj];
		            fnt -> lon[jj] = tmplon[jj];
		        }
			
			if ( idxdir[jj] >= 180.0F ) {
			    idxdir[jj] -= 180.0F;
			}
			else if ( idxdir[jj] >= 0.0F ) {
			    idxdir[jj] += 180.0F;
			}
		        fnt -> fntdir[jj] = idxdir[jj];

			if ( idxspd[jj] > 0 || 
			     ((fabs(idxspd[jj]) < dlta) && (idxdir[jj] < 0.0F)) ) {
		            fnt -> fntspd[jj] = pr_knms ( &idxspd[jj] );
			}
			else {
			    fnt -> fntspd[jj] = SIGRLMS;
			}
	            }
	        }
		skip = False;
	    } 
        }
	else if ( ( ieloff == IMISSD ) || ( ier != 0 ) ) {
	    ier = 0;
	    done = True; 
	    if ( numfnt == 0 ) {
	        ier = 1;
                er_lmsg ( &level, sggrp, &ier, gtstr, &ier1,
	                  strlen(sggrp), strlen(gtstr) );
	    }
	    if ( itcznum == 0 ){
		rewind (fptr);
	        done = False; 
		itcznum = 1;
                vgclss = CLASS_LINES;
                vgnum  = SPLN_ELM;
           /*     sbtyp  = ITCZ_LINE;   */
                sbtyp  = -99;
                istoff  = 0;
	    }
	}
	gpold = gpnum;
    }

   /* 
    * Write out FRONT information in BUFR ASCII format.
    */

    if ( ier == 0 ) {
        ptr = head;
        cas_wrfrt ( ifpout, ptr, numfnt, chlvl, &ier );
    }
   /*
    * Free memory from the linked list.
    */

    if ( memalc ) {
        ptr = head;
        ptr2 = ptr -> next;
        while ( ptr2 != NULL ) {
	    free (ptr);
            ptr = ptr2;
	    ptr2 = ptr2 -> next;
        }
        free ( ptr );
    }

   /*
    * Free memory from temporary group number.
    */

    free ( tmpgrp );

}
