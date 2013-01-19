#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

void ctb_permccrd ( char *tblnam, char *dirsym, Permclust_t *pc, int *iret )
/************************************************************************
 * ctb_permccrd								*
 *									*
 * This routine will read the permanent clustered county table into a 	*
 * structure.								*
 *									*
 * ctb_permccrd ( tblnam, dirsym, pc, iret )				*
 *									*
 * Input parameters:							*
 *	*tblnam		char		Data type table name		*
 *	*dirsym		char		Directory			*
 *									*
 * Output parameters:							*
 *	*pc	Permclust_t 		Perm. clustered cnty structure	*
 *	*iret	int			Return code			*
 **									*
 * Log:									*
 * A. Hardy/NCEP	10/04		copied from ctb_ccrd		*
 * A. Hardy/NCEP        1/05	Added creation of virtual cluster combos*
 * 			      	& added error check on 1st cst_split    *
 ***********************************************************************/
{
    FILE    *ftbl;
    char    buff[256], pcname[32], *next;
    int	    ii, jj, ij, in, inx, numclust, fips[50], nfips, len, ier, ierr;
    int     ik, kk, ll, nn, itotal, srchfip, inum, ivfips, cfips[20];
    Boolean	found, match;
    static Permclust_t  vpc, tpc;

/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     *  Open the table.
     */
    ftbl = cfl_tbop ( tblnam, dirsym, iret );
    if ( *iret != 0 )  {
        tpc.nclust = 0;
        return;
    }

    /*
     *  Get number of valid table entries.
     */
    cfl_tbnr( ftbl, &numclust, &ier );

    if ( numclust != 0 )  {
        /*
         *  Allocate the structure elements.
         */
        tpc.nclust = numclust;
        tpc.clust = (PCinfo *) malloc( numclust * sizeof(PCinfo) );
    }
    else  {
        /*
         *  Problem opening table file; set error code and return.
         */
        cfl_clos( ftbl, &ier );
        *iret = -2;
        return;
    }

    rewind ( ftbl );

    /*
     *  For every valid table entry, read in, parse, put in structure
     */

    ii = 0;
    while ( ii < numclust )  {

	cfl_trln( ftbl, sizeof(buff), buff, &ier );

	if ( ier == 0 )  {

	    next = cst_split ( buff, '|', 4, tpc.clust[ii].pcwfo, &ier );

            if ( ier == 0 ) {
	        next = cst_split ( next, '|', 32, pcname, &ier );
	        tpc.clust[ii].pcname = 
		    (char *)malloc( (strlen(pcname)+1) * sizeof(char) );
	        strcpy ( tpc.clust[ii].pcname, pcname );

	        cst_rmbl ( next, next, &len, &ier );

	        cst_ilst ( next, '+', IMISSD, sizeof(fips)/sizeof(fips[0]),
		           fips, &nfips, &ier );

	        tpc.clust[ii].npc = nfips;
                tpc.clust[ii].pc = (int *)malloc( nfips * sizeof(int) );

	        for ( jj = 0; jj < nfips; jj++ )  {
		    tpc.clust[ii].pc[jj] = fips[jj];
	        }
            }
            else {
                ierr = 3; 
                er_wmsg ( "CTB", &ierr, buff, &ier, 3, strlen (buff) );
            }
	    ii++;

	}

    }

    cfl_clos ( ftbl, &ier );

   /*
    * Create virtual clusters.
    * Allocate the structure elements.
    */

    vpc.nclust = 0;
    vpc.clust = (PCinfo *) malloc( (numclust*4) * sizeof(PCinfo) );
    inum = vpc.nclust;
    ii = 0;
   /*
    * Loop over all perm clusters.
    */
    while ( ii < numclust ) {
        for ( jj = 1;jj < tpc.clust[ii].npc; jj++ ) {
            found = False;
            srchfip = tpc.clust[ii].pc[jj];
           /*
            * Compare search fip value to the other 1st fips codes.
            * Search permanent table first.
            */
            ij = 0;
            while ( (!found) && (ij < numclust ) ) {
               if ( srchfip == tpc.clust[ij].pc[0] ) {
                   found = True;
               }
               ij++;
            }
           /*
            * Search virtual cluster table next.
            */
            ij = 0;
            while ( (!found) && (ij < vpc.nclust ) ) {
                if ( srchfip == vpc.clust[ij].pc[0] ) {
                    found = True;
                }
                ij++;
            }

           /*
            * Didn't find a cluster group with search fip as first key.
            * Find all cluster groups in perm. table with this key in 
            * the cluster groups. Create a new virtual cluster entry.
            */

            if ( !found ) {
                cfips[0] = srchfip;
                ik = 0;
               /* Loop over rest of current clustered combo fips 
                * and store the codes temporarily.
                */
                for ( ij = 0; ij < tpc.clust[ii].npc;ij++ ) {
                    if ( tpc.clust[ii].pc[ij] != srchfip ) {
                       ik++;
                       cfips[ik] = tpc.clust[ii].pc[ij];
                    }
                }
               /* Set the number of virtual fips codes we have so far*/
                ivfips = ik+1;

               /* 
                * Check rest of perm clusters combox for srchfip.
                * Start with the next perm cluster combo.
                */
                inx = ii + 1;
                match = False;
                    /* loop over rest of combo clusters */
                    for ( kk= inx; kk < numclust; kk++ )  {
                        /* loop over number of counties in each cluster */
                        for ( ll = 0; ll < tpc.clust[kk].npc; ll++ )  {
                            /* look for a match in a cluster */
                            if ( tpc.clust[kk].pc[ll] == srchfip )  {
                                /* store all new codes in cfips array */
                                for ( nn = 0; nn < tpc.clust[kk].npc; nn++ )  {
                                    /* loop over current cluster array, store
                                       one's we don't have*/
                                    in = 0;
                                    while ( (in < ivfips ) && ( !match) ) {
                                        if ( tpc.clust[kk].pc[nn] == cfips[in] )  {
                                            match = True; 
                                        }
                                        in++;
                                    }
                                   /* didn't find fips in cfips array, keep it */
                                    if ( !match ) { 
                                        cfips[ivfips]  = tpc.clust[kk].pc[nn];
                                        ivfips++;
                                    }
                                    match = False;
                                }
                            }
                        }
                    }

               /*
                * Put temporary pcname, pcwfo, npc and fips array into 
                * virtual perm clust. combos.
                * Increment number of virtual fip cluster combos
                */

                 strcpy ( vpc.clust[inum].pcwfo, tpc.clust[ii].pcwfo );
             
	         vpc.clust[inum].pcname = 
		        (char *)malloc( (strlen(pcname)+1) * sizeof(char) );

                /*
                 * Store the number of fips codes and the code numbers
                 */
      
                  vpc.clust[inum].npc = ivfips;
                  vpc.clust[inum].pc = (int *)malloc( ivfips * sizeof(int) );
                  for ( ij= 0; ij < ivfips; ij++ )  {
                      vpc.clust[inum].pc[ij] = cfips[ij];
                  }
                  vpc.nclust++;
                  inum++;
            } /* (!found) loop */ 
        } /* for jj loop */
        ii++;
    } /* while ii loop */ 

   /*
    * Fill out output permanent cluster structure.
    * Allocate the structure elements.
    */

    itotal = numclust + vpc.nclust;
    pc->nclust = itotal;
    pc->clust = (PCinfo *) malloc( itotal * sizeof(PCinfo) );
    ii = 0;

   /*
    * Write out permanent table cluster combos first.
    */
    while ( ii < numclust )  {

        strcpy (pc->clust[ii].pcwfo, tpc.clust[ii].pcwfo);
        pc->clust[ii].pcname = 
                    (char *)malloc( (strlen(tpc.clust[ii].pcname)+1) * sizeof(char) );
        strcpy ( pc->clust[ii].pcname, tpc.clust[ii].pcname );

        pc->clust[ii].npc = tpc.clust[ii].npc;
        pc->clust[ii].pc = (int *)malloc( tpc.clust[ii].npc * sizeof(int) );
        for ( jj = 0; jj < pc->clust[ii].npc; jj++ )  {
            pc->clust[ii].pc[jj] =  tpc.clust[ii].pc[jj];
        }
        ii++;
    }
    
   /*
    * Write out virtual cluster combos next.
    */
    jj = 0;
    while ( ii < pc->nclust )  {

        strcpy (pc->clust[ii].pcwfo, vpc.clust[jj].pcwfo);
        pc->clust[ii].pcname =
                    (char *)malloc( 12 * sizeof(char) );
        sprintf ( pc->clust[ii].pcname, "VClust %d", jj+1 );

        pc->clust[ii].npc = vpc.clust[jj].npc;
        pc->clust[ii].pc = (int *)malloc( vpc.clust[jj].npc * sizeof(int) );
        for ( ij = 0; ij < pc->clust[ii].npc; ij++ )  {
            pc->clust[ii].pc[ij] =  vpc.clust[jj].pc[ij];
        }
        ii++;
        jj++;
    }

} 
