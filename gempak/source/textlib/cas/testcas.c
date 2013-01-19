#include "geminc.h" 
#include "gemprm.h"
#include "cascmn.h"

int main ( void )
/************************************************************************
 * TESTCAS								*
 *									*
 * This program tests the CGEMLIB "CAS" read functions.			*
 *									*
 **									*
 * Log:									*
 * A. Hardy/SAIC	02/02	Created					*
 * M. Li/SAIC		04/04	Added flight level deltas for jets	*
 * M. Li/SAIC		09/04	Added CAS_RDMCLD			*
 * M. Li/SAIC		01/05	Removed 'High' from 'High Sig. Wx ASCII'*
 * S. Jacobs/NCEP	 8/05	Set ifpout to NULL after closing	*
 ***********************************************************************/
{

    int		ii, ij, numsub, numele, ier, iret;
    int         itime[5], jtime[5], memalc, pos, idcent;
    int         rnum, lnum, hnum, membx, memhi, memlo;
    float	chbase, chtop;
    char	ofname[256], select[LLSCRN], name[40], cc[2];
    char        tmpln[256], tmpst[10];
    Boolean	cont, readflg;

    cloud_t     *ptr, *ptrc, *head, *ptr2;
    mcloud_t    *mptr, *ptrm, *headm, *ptr2m;
    jets_t      *jetp, *ptrj, *headj, *ptr2j;
    front_t     *frtp, *ptrf, *headf, *ptr2f;
    turb_t      *trbp, *ptrb, *headb, *ptr2b;
    storm_t     *stmp, *ptrs, *heads, *ptr2s;
    volrad_t    *vrdp, *ptrv, *headv, *ptr2v;
    trop_t      *trpp, *ptrr, *headr, *ptr2r;
    trophi_t    *thip, *ptrh, *headh, *ptr2h;
    troplo_t    *tlop, *ptrl, *headl, *ptr2l;

    FILE        *ifpout;
/*---------------------------------------------------------------------*/
	iret   = 0;
	cont   = False;
	ifpout = NULL;
	strcpy ( cc, "_");

	while ( cont == False ) {
	    printf ( "\n\n" );
	    printf ( "   1 = CAS_OPEN    2 = CAS_CLOS   3 = CAS_RDHDR\n"  );
	    printf ( "   4 = CAS_RDCLD   5 = CAS_RDFRT  6 = CAS_RDJETS\n" );
	    printf ( "   7 = CAS_RDSTM   8 = CAS_RDTROP 9 = CAS_RDTURB\n" );
	    printf ( "  10 = CAS_RDVLRD 11 = CAS_RDMCLD \n\n");
	    printf ( "\n" );
	    printf ( "Select a subroutine number or type EXIT: " );
	    scanf ( " %s", select );
	    switch ( select[0] ) {
		case 'e':
		case 'E':
			cont = True;
		default:
	                numele = 0;
	                memalc = 0;
			numsub = atoi ( select );
			break;
	    }

/*---------------------------------------------------------------------*/
	    if ( numsub == 1 ) {

		printf("Enter the Sig. Wx ASCII filename. \n");
		scanf( " %s", ofname );

		readflg = True;
		ifpout = (FILE *)cas_open ( ofname, readflg, &iret );

		printf ( "\nCAS_OPEN: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 2 ) {

		cas_clos ( ifpout, &ier );
		ifpout = NULL;

		printf ( "\nCAS_CLOS: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 3 ) {

		if ( ifpout == NULL ) {
		    printf("\n Open an ASCII file.\n\n");
		}
		else {
		    rewind (ifpout);
		    cas_rdhdr ( ifpout, itime, jtime, &idcent, &chbase, &chtop, &iret );
		    printf ( "\nIssue time : %d %d %d %d %d \n",
		           itime[0], itime[1], itime[2], itime[3], itime[4]);
		    printf ( "Valid time : %d %d %d %d %d \n",
		           jtime[0], jtime[1], jtime[2], jtime[3], jtime[4]);
		    printf ( "Originating center id: %d\n", idcent );
		    printf ( "Base/top level of chart: %f/%f \n\n", chbase, chtop ); 

		    printf ( "\nCAS_RDHDR: iret = %d\n\n", iret );
		}
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 4 ) {

		if ( ifpout == NULL ) {
		    printf("\n Open an ASCII file.\n\n");
		}
		else {
		    rewind (ifpout);
		    cas_rdcld ( ifpout, &numele, &ptrc, &memalc, &iret );

		    if ( numele == 0 ) {
		        printf ( "\nCLOUD elements not found.\n");
		    }
		    else {
		        printf ( "Number of CLOUD groups:  %d  \n", 
			                     numele);
			if ( memalc == 1) {
        		    ptr = ptrc;
                            for ( ij = 0; ij < numele; ij++ ) {
                                printf("\nHeights: %7.1f   %7.1f\n", 
			                     ptr->level1, ptr->level2);
                                printf("Lat/Lon pairs: %d \n", ptr -> npt);
                                for ( ii = 0; ii < ptr->npt; ii++ ) {
                                    printf(" %7.1f   %7.1f\n", 
			                   ptr->lat[ii], ptr->lon[ii]);
			        }
                                printf("Distribution: %d   Type: %d\n", 
			                   ptr->clddist, ptr->cldtyp);
        		        ptr = ptr -> next;
			    }

			   /*
			    * Free link list.
			    */

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
		    printf ( "\nCAS_RDCLD: iret = %d\n\n", iret );
		}
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 5 ) {
                if ( ifpout == NULL ) {
		    printf("\n Open an ASCII file.\n\n");
		}
		else {
		    rewind (ifpout);
		    cas_rdfrt ( ifpout, &numele, &ptrf, &memalc, &iret );
		    if ( numele == 0 ) {
		        printf ( "\nFRONT elements not found.\n");
		    }
		    else {
		        printf ( "\nNumber of FRONT groups:  %d  \n", 
			                     numele);
                        if ( memalc == 1) {
        		    frtp = ptrf;
                            for ( ij = 0; ij < numele; ij++ ) {
				printf("Front type: %d \n", frtp->ftype);
                                printf("Lat/Lon pairs: %d \n", frtp -> npt);
                                for ( ii = 0; ii < frtp -> npt; ii++ ) {
                                    printf(" %7.1f   %7.1f", 
			                   frtp->lat[ii], frtp->lon[ii]);
                                    printf("   %10.1f    %10.1f\n", 
			                   frtp->fntdir[ii], frtp->fntspd[ii]);
			        }
        		        frtp = frtp -> next;
			    }

			   /*
			    * Free link list.
			    */

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
		    printf ( "\nCAS_RDFRT: iret = %d\n\n", iret );
		}
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 6 ) {
	        if ( ifpout == NULL ) {
		    printf("\n Open an ASCII file.\n\n");
		}
		else {
		    rewind (ifpout);
		    cas_rdjets ( ifpout, &numele, &ptrj, &memalc, &iret );
		    if ( numele == 0 ) {
		        printf ( "\nJET elements not found.\n");
		    }
		    else {
		        printf ( "\nNumber of JET groups:  %d  \n", 
			                     numele);
                        if ( memalc == 1) {
        		    jetp = ptrj;
                            for ( ij = 0; ij < numele; ij++ ) {
                                printf("Lat/Lon pairs: %d \n", jetp -> npt);
                                for ( ii = 0; ii < jetp -> npt; ii++ ) {
                                    printf(" %7.1f   %7.1f", 
			                   jetp->lat[ii], jetp->lon[ii]);
                                    printf("    %10.1f    %6.1f   %10.1f    %10.1f\n", 
			                   jetp->level[ii], jetp->speed[ii], 
				           jetp->levabv[ii], jetp->levblw[ii] );
			        }
        		        jetp = jetp -> next;
			    }

			   /*
			    * Free link list.
			    */

        		    headj = ptrj;
        		    ptr2j = headj -> next;
        		    while ( ptr2j != NULL ) {
            		        free ( headj );
            		        headj = ptr2j;
            		        ptr2j = ptr2j -> next;
        		    }
        		    free ( headj );
			}
		    }
		    printf ( "\nCAS_RDJETS: iret = %d\n\n", iret );
		}
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 7 ) {
                if ( ifpout == NULL ) {
		    printf("\n Open an ASCII file.\n\n");
		}
		else {
		    rewind (ifpout);
		    cas_rdstm ( ifpout, &numele, &ptrs, &memalc, &iret );
		    if ( numele == 0 ) {
		        printf ( "\nSTORM elements not found.\n");
		    }
		    else {
		        printf ( "\nNumber of STORMS :  %d  \n", numele);
                        if ( memalc == 1) {
        		    stmp = ptrs;
                            for ( ij = 0; ij < numele; ij++ ) {
                                printf("\nName: %s \n", stmp->name);
                                printf("Location: %5.1f   %7.1f\n", 
			                   stmp->lat, stmp->lon);
                                printf("Type: %d   \n", stmp->stmtyp);
        		        stmp = stmp -> next;
			    }

			   /*
			    * Free link list.
			    */

        		    heads = ptrs;
        		    ptr2s = heads -> next;
        		    while ( ptr2s != NULL ) {
            		        free ( heads );
            			heads = ptr2s;
            			ptr2s = ptr2s -> next;
        		    }
        		    free ( heads );
                        }
		    }
		    printf ( "\nCAS_RDSTM: iret = %d\n\n", iret );
		}
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 8 ) {
                if ( ifpout == NULL ) {
		    printf("\n Open an ASCII file.\n\n");
		}
                else {
		    rnum = 0;
		    lnum = 0;
		    hnum = 0;
		    membx = 0;
		    memhi = 0;
		    memlo = 0;

		    rewind (ifpout);
		    cas_rdtrop ( ifpout, &numele, &ptrr, &membx, &rnum, 
		                 &ptrl, &memlo, &lnum, &ptrh, &memhi, 
				 &hnum, &iret );
		    if ( numele == 0 ) {
		        printf ( "\nTROPOPAUSE elements not found.\n");
		    }
		    else {
		        printf ( "\nNumber of TROPOPAUSE elements:  %d\n", 
			                     numele);
                        if ( membx == 1) {
        		    trpp = ptrr;
                            printf("Number of REGULAR trops. : %d \n", rnum);
                            for ( ij = 0; ij < rnum; ij++ ) {
                                printf(" Location: %7.1f   %7.1f    ", 
			                   trpp->lat, trpp->lon);
                                printf("Height: %7.1f   \n", trpp->level);
        		        trpp = trpp -> next;
			    }

			   /*
			    * Free link list.
			    */

                            headr = ptrr;
        		    ptr2r = headr -> next;
        		    while ( ptr2r != NULL ) {
            		        free ( headr );
            			headr = ptr2r;
            			ptr2r = ptr2r -> next;
        		    }
        		    free ( headr );
			}

                        if ( memhi == 1) {
        		    thip = ptrh;
                            printf("Number of HIGH trops. : %d \n", hnum);
                            for ( ij = 0; ij < hnum; ij++ ) {
                                printf(" Location: %7.1f   %7.1f    ", 
			                   thip->lat, thip->lon);
                                printf("Height: %7.1f   \n", thip->level);
        		        thip = thip -> next;
			    }

			   /*
			    * Free link list.
			    */

                            headh = ptrh;
        		    ptr2h = headh -> next;
        		    while ( ptr2h != NULL ) {
            		        free ( headh );
            			headh = ptr2h;
            			ptr2h = ptr2h -> next;
        		    }
        		    free ( headh );
			}

                        if ( memlo == 1) {
        		    tlop = ptrl;
                            printf("Number of LOW trops. : %d \n", lnum);
                            for ( ij = 0; ij < lnum; ij++ ) {
                                printf(" Location: %7.1f   %7.1f    ", 
			                   tlop->lat, tlop->lon);
                                printf("Height: %7.1f \n", tlop->level);
        		        tlop = tlop -> next;
			    }

			   /*
			    * Free link list.
			    */

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
		    printf ( "\nCAS_RDTROP: iret = %d\n\n", iret );
		}
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 9 ) {

	        if ( ifpout == NULL ) {
		    printf("\n Open an ASCII file.\n\n");
		}
		else {
		    rewind (ifpout);
		    cas_rdturb ( ifpout, &numele, &ptrb, &memalc, &iret );
		    if ( numele == 0 ) {
		        printf ( "\nTURB elements not found.\n");
		    }
		    else {
		        printf ( "\nNumber of TURBULENCE groups:  %d\n", 
			                     numele);
		        if ( memalc == 1) {
        		    trbp = ptrb;
                            for ( ij = 0; ij < numele; ij++ ) {
                                printf("\nHeights  %7.1f   %7.1f\n", 
			                     trbp->level1, trbp->level2);
                                printf("Lat/Lon pairs: %d \n", trbp -> npt);
                                for ( ii = 0; ii < trbp->npt; ii++ ) {
                                    printf(" %7.1f   %7.1f\n", 
			                   trbp->lat[ii], trbp->lon[ii]);
			        }
                                printf("Degree : %d   \n", trbp->tdeg);
        		        trbp = trbp -> next;
			    }

			   /*
			    * Free link list.
			    */

        		    headb = ptrb;
        		    ptr2b = headb -> next;
        		    while ( ptr2b != NULL ) {
            		        free ( headb );
            			headb = ptr2b;
            			ptr2b = ptr2b -> next;
        		    }
        		    free ( headb );
			}

		    }
		    printf ( "\nCAS_RDTURB: iret = %d\n\n", iret );
		}
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 10 ) {

	        if ( ifpout == NULL ) {
		    printf("\n Open an ASCII file.\n\n");
		}
		else {
		    rewind (ifpout);
		    cas_rdvlrd ( ifpout, &numele, &ptrv, &memalc, &iret );
		    if ( numele == 0 ) {
		        printf ( "\nVOLCANO/RADIATION elements not found.\n");
		    }
		    else {
		        printf ( "Number of VOLCANO/RADIATION groups:  %d  \n", 
			                     numele);
                        if ( memalc == 1) {
        		    vrdp = ptrv;
                            for ( ij = 0; ij < numele; ij++ ) {
				cst_rmst ( vrdp ->name, cc, &pos, name, &ier );
				if ( pos <  0 ) {
                                    printf("\nVolcano name : %s \n", vrdp->name);
			        }
				else {
                                    printf("\nRadiation name : %s \n", name);
				}
                                printf("Location: %5.1f   %7.1f\n", 
			                   vrdp ->lat, vrdp ->lon);
				printf("Date : %7d   %7d  %7d  %7d  %7d \n", 
					vrdp->year, vrdp->month, vrdp->day, 
					vrdp->hour, vrdp->minute);
        		        vrdp = vrdp -> next;
			    }

			   /*
			    * Free link list.
			    */

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
		    printf ( "\nCAS_RDVLRD: iret = %d\n\n", iret );
		}
	    }
/*---------------------------------------------------------------------*/
            if ( numsub == 11 ) {

                if ( ifpout == NULL ) {
                    printf("\n Open an ASCII file.\n\n");
                }
                else {
                    rewind (ifpout);
                    cas_rdmcld ( ifpout, &numele, &ptrm, &memalc, &iret );

                    if ( numele == 0 ) {
                        printf ( "\nMCLOUD elements not found.\n");
                    }
                    else {
                        printf ( "Number of MCLOUD groups:  %d  \n",
                                             numele);
                        if ( memalc == 1) {
                            mptr = ptrm;

			   /*
        		    * Lat/lon.
        		    */

        		    printf("Lat/Lon pair: %d \n", mptr -> npt );
        		    for ( ii = 0; ii < mptr -> npt; ii++ ) {
            			printf ("%5.1f      %6.1f\n",
                                           mptr->lat[ii], mptr->lon[ii] );
        		    }
			
			   /*
        		    * Non-Cb cloud distribution.
        		    */

        		    printf("Number of Non-Cb cloud distribution: %d \n", 
				   mptr -> ncld );
        		    if ( mptr -> ncld > 0 ) {
            		    	sprintf ( tmpln, "%d      ", mptr -> ncdis[0] );
            		    	for ( ii = 1; ii < mptr -> ncld; ii++ ) {
                		    cst_inch ( mptr -> ncdis[ii], tmpst, &ier );
                		    strcat ( tmpln, tmpst );
                		    strcat ( tmpln, "      " );
            		        }

            		        printf ("Non-Cb cloud distribution: %s \n", tmpln );
        		    }

			   /*
        		    * Non-Cb cloud type.
        		    */

        		    printf("Number of non-Cb type: %d \n", mptr -> ntyp );
        		    if ( mptr -> ntyp > 0 ) {
            			sprintf ( tmpln, "%d      ", mptr -> nctyp[0] );
            			for ( ii = 1; ii < mptr -> ntyp; ii++ ) {
                		    cst_inch ( mptr -> nctyp[ii], tmpst, &ier );
                		    strcat ( tmpln, tmpst );
                		    strcat ( tmpln, "      " );
            			}

           			printf ("Non-Cb Cloud type: %s \n", tmpln );
        		    }

			   /*
        		    * Turbulence.
            		    */

        		    printf( "Turbulence flag: %d \n", mptr -> turb );
        		    if ( mptr -> turb == 1 ) {
            			printf( "Turbulence base/top: %10.1f/%10.1f \n",
                        	    	mptr -> tbase, mptr -> ttop );
            			printf( "Degree of turbulence: %d \n", mptr -> tdeg );
        		    }

			   /*
        		    * Icing.
        		    */

        		    printf( "Icing flag: %d \n", mptr -> icing);
        		    if ( mptr -> icing == 1 ) {
            			printf( "Icing base/top: %10.1f/%10.1f \n",
                        		mptr -> icbase, mptr -> ictop );
            			printf("Degree of icing: %d \n", mptr -> dic );
        		    }

			   /*
        		    * Cb.
        		    */
        		    printf( "Cb flag: %d \n", mptr -> fcb );
        		    if ( mptr -> fcb == 1 ) {
            			printf( "Cb base/top: %10.1f/%10.1f \n",
                        		mptr -> cbbase, mptr -> cbtop );
            			printf("Cb distribution: %d, Cb Type: %d \n",
                        		mptr -> cbdis, mptr -> cbtyp );
        		    }
                
                           /*
                            * Free link list.
                            */ 
                        
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
                    printf ( "\nCAS_RDMCLD: iret = %d\n\n", iret );
                }
            }
/*---------------------------------------------------------------------*/
	}

	if  ( ifpout != NULL )  fclose ( ifpout );
	return 0;
}
