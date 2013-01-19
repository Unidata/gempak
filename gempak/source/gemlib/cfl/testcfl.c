#include "geminc.h" 
#include "gemprm.h"

int main ( void )
/************************************************************************
 * TESTCFL								*
 *									*
 * This program tests the GEMLIB "CFL" functions.			*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	 3/96						*
 * G. Krueger/EAI	 8/96	Added CFL_MDAT; Changed CFL_TOPN->TBOP	*
 * D.W.Plummer/NCEP	12/96	Added CFL_SCND				*
 * D.W.Plummer/NCEP	 2/97	Changed calling sequence to CFL_SCND	*
 * G. Krueger/EAI	 9/97	Added CFL_RDIR and CFL_DOPN		*
 * D.W.Plummer/NCEP	 3/98	Added CFL_TBNR				*
 * D.W.Plummer/NCEP	 4/98	Updated for CFL_TBNR calling sequence	*
 *				change					*
 * S. Jacobs/NCEP	 4/98	Fixed a bug when calling CFL_RDIR	*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * S. Jacobs/NCEP	 6/99	Added sort order to CFL_SCND		*
 * S. Jacobs/NCEP	 9/99	Changed call to CFL_MDAT		*
 * S. Jacobs/NCEP	11/99	Added CFL_PATH				*
 * M. Li/GSC		 5/00	Increased length of filstr		*
 * S. Jacobs/NCEP	 5/01	Added CFL_TINQ				*
 * E. Safford/SAIC	04/02	added CFL_ISDIR				*
 * T. Piper/SAIC	05/02	added CFL_PERMS				*
 * H. Zeng/SAIC		01/05	modified CFL_RDIR			*
 * R. Tian/SAIC		 1/06	added CFL_SCNT				*
 * T. Piper/SAIC	04/07	Added cfl_gfil				*
 * T. Piper/SAIC	04/07	Modified for cfl_scnt CSC		*
 ***********************************************************************/
{
	int		ii, cont, nr, nsdir;
	int		iret, numsub, nbytes, bufspc, i, ival, nin,
			ierrno, iflerr, idir, iorigin, ifrom, ifdes,
			irdtyp, isrflg;
	long		loffset, lflen, lfaddr;
	char		select[LLSCRN], filnam[LLSCRN], ddname[LLSCRN],
			sdname[LLSCRN], tblpath[LLSCRN], pattern[LLSCRN],
			fullpath[LLSCRN], ans[LLSCRN], dattim[LLSCRN],
			template[LLSCRN], search[LLSCRN], defdat[LLSCRN],
			basnam[LLSCRN], dirnam[LLSCRN];
	char		*defdir, sep[2]=";", *subdir;
	char		flist[MXNMFL][MXFLSZ],filstr[MXNMFL*MXFLSZ];
	int		plen, flen, tlen, nf, maxlen, order;
	Boolean		can_read, can_write;
	unsigned char	buffer[LLBSIZ];
	FILE		*filptr = NULL;
	struct dirent	**dnlist=NULL;

/*---------------------------------------------------------------------*/
	cont = G_FALSE;

	while ( cont == G_FALSE ) {
	    printf ( "\n\n" );
	    printf ( "   1 = CFL_ROPN   2 = CFL_AOPN   3 = CFL_UOPN\n" );
	    printf ( "   4 = CFL_TBOP   5 = CFL_WOPN   6 = CFL_TMPO\n" );
	    printf ( "   7 = CFL_CLOS   8 = CFL_DOPN   9 = CFL_PATH\n" );
	    printf ( "  10 = CFL_GFIL  11 = CFL_WRIT  12 = CFL_READ\n" );
	    printf ( "  13 = CFL_RDLN  14 = CFL_TRLN  15 = CFL_PERMS\n" );
	    printf ( "  21 = CFL_SEEK  22 = CFL_SRCH  23 = CFL_IRET\n" );
	    printf ( "  24 = CFL_INQR  25 = CFL_MNAM  26 = CFL_WHER\n" );
	    printf ( "  27 = CFL_MDAT  28 = CFL_SCND  29 = CFL_RDIR\n" );
	    printf ( "  30 = CFL_TBNR  31 = CFL_TINQ  32 = CFL_ISDIR\n" );
	    printf ( "  33 = CFL_SCNT\n\n" );
	    printf ( "\n" );
	    printf ( "Select a subroutine number or type EXIT: " );
	    scanf ( " %s", select );
	    switch ( select[0] ) {
		case 'e':
		case 'E':
			cont = G_TRUE;
		default:
			numsub = atoi ( select );
			break;
	    }

/*---------------------------------------------------------------------*/
	    if ( numsub == 1 ) {

		printf ( "Enter the file name:\n" );
		scanf ( " %s", filnam );
		printf ( "Is there a default directory (y/n):\n" );
		scanf ( " %s", ans );
		if  ( ans[0] == 'y' || ans[0] == 'Y' ) {
		    printf ( "Enter the default directory:\n" );
		    defdir = ddname;
		    scanf ( " %s", defdir );
		} else {
		    defdir = NULL;
		}

		filptr = cfl_ropn ( filnam, defdir, &iret );

		printf ( "\nCFL_ROPN: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 2 ) {

		printf ( "Enter the file name:\n" );
		scanf ( " %s", filnam );

		filptr = cfl_aopn ( filnam, &iret );

		printf ( "\nCFL_AOPN: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 3 ) {

		printf ( "Enter the file name:\n" );
		scanf ( " %s", filnam );

		filptr = cfl_uopn ( filnam, &iret );

		printf ( "\nCFL_UOPN: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 4 ) {

		printf ( "Enter the table name:\n" );
		scanf ( " %s", tblpath );
		printf ( "Is there a subdirectory (y/n):\n" );
		scanf ( " %s", ans );
		if  ( ans[0] == 'y' || ans[0] == 'Y' ) {
		    printf ( "Enter the subdirectory:\n" );
		    subdir = sdname;
		    scanf ( " %s", subdir );
		} else {
		    subdir = NULL;
		}

		filptr = cfl_tbop ( tblpath, subdir, &iret );

		printf ( "\nCFL_TBOP: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 5 ) {

		printf ( "Enter the file name:\n" );
		scanf ( " %s", filnam );

		filptr = cfl_wopn ( filnam, &iret );

		printf ( "\nCFL_WOPN: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 6 ) {

		filptr = cfl_tmpo ( &iret );

		printf ( "\nCFL_TMPO: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 7 ) {

		cfl_clos ( filptr, &iret );

		printf ( "\nCFL_CLOS: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 8 ) {

		printf ( "Enter the file name:\n" );
		scanf ( " %s", filnam );

		cfl_dopn ( filnam, &ifdes, &iret );

		if ( iret >= 0 ) close ( ifdes );

		printf ( "\nCFL_DOPN: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 9 ) {

		printf ( "Enter the full path name:\n" );
		scanf ( " %s", fullpath );

		cfl_path ( fullpath, dirnam, basnam, &iret );

		if  ( iret >= 0 )  {
		    printf ( "dirnam = %s\n", dirnam );
		    printf ( "basnam = %s\n", basnam );
		}

		printf ( "\nCFL_PATH: iret = %d\n\n", iret );

	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 10 ) {

		printf ( "Enter sort type ( 0 = name, 1 = date):\n" );
		scanf ( " %d", &order );

                printf ( "Enter maximum number of files:\n" );
                scanf ( " %d", &bufspc );

                printf ("Enter the full file path:\n");
                scanf ( " %s", fullpath );

                printf ( "Do you have a search string (0=No, 1=Yes):\n" );
                scanf ( " %d", &isrflg );
                if ( isrflg != 0 ) {
                    printf ( "Enter a search string:\n" );
                    scanf ( " %s", search );
                }
                else {
                    search[0] = '\0';
                }

		nf = cfl_gfil(order, bufspc, fullpath, search, flist);
                if ( nf > 0 ) {
                    printf ( "\n Number of entries = %d\n", nf );
                    for ( ii = 0; ii < nf; ii++ ) {
                        printf ( "  - %s\n", flist[ii] );
                    }
                } else {
                    printf ( "No files found.\n" );
                }
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 11 ) {

		printf ( "Enter length in bytes:\n" );
		scanf ( " %d", &nbytes );

		printf ( "Enter %d data values:\n", nbytes );
		for ( i = 0; i < nbytes; i++ ) {
		    scanf ( " %d", &ival );
		    buffer[i] = ival;
		}

		cfl_writ ( filptr, nbytes, buffer, &iret );

		printf ( "\nCFL_WRIT: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 12 ) {

		printf ( "Enter length in bytes:\n" );
		scanf ( " %d", &nbytes );

		cfl_read ( filptr, nbytes, buffer, &nin, &iret );

		if ( iret >= 0 ) {
		    printf ( "\nCFL_READ: nbin = %d, buffer =\n", nin );
		    for ( i = 0; i < nin; i++ ) {
			ival = buffer[i];
			printf ( " %3.3u", ival );
			if ( ( (i+1) % 16 ) == 0 ) {
			    printf ( "\n" );
			}
		    }
		}

		printf ( "\nCFL_READ: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 13 ) {

		printf ( "Enter maximum length in characters:\n" );
		scanf ( " %d", &bufspc );

		cfl_rdln ( filptr, bufspc, (char *)buffer, &iret );

		if ( iret >= 0 )
		    printf ( "\nCFL_RDLN: buffer = %s\n", buffer );
		printf ( "CFL_RDLN: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 14 ) {

		printf ( "Enter maximum length in characters:\n" );
		scanf ( " %d", &bufspc );

		cfl_trln ( filptr, bufspc, (char *)buffer, &iret );

		if ( iret >= 0 )
		    printf ( "\nCFL_TRLN: buffer = %s\n", buffer );
		printf ( "CFL_TRLN: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 15 ) {

		printf ( "Enter file name to check for permissions:\n" );
		scanf ( " %s", fullpath );

		cfl_perms ( fullpath, &can_read, &can_write, &iret );

		printf ( "\nCFL_PERMS: can_read = %d, can_write = %d\n\n", can_read, can_write );
	    }

/*---------------------------------------------------------------------*/ 
	    else if ( numsub == 21 ) { 
	
                printf ( "Enter offset:\n" );
                scanf ( " %ld", &loffset );

                printf ( "Enter origin (0=begin, 1=current, 2=end):\n" );
                scanf ( " %d", &iorigin );
                if ( iorigin == 0 ) {
                    ifrom = SEEK_SET;
		} else if ( iorigin == 1 ) {
		    ifrom = SEEK_CUR;
		} else if ( iorigin == 2 ) {
		    ifrom = SEEK_END;
		} else {
		    ifrom = SEEK_SET;
		}

		cfl_seek ( filptr, loffset, ifrom, &iret );

		printf ( "\nCFL_SEEK: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 22 ) {

		printf ( "Enter the pattern to find:\n" );
		scanf ( " %s", pattern );

		printf ( "Enter direction (0=forward, 1=reverse):\n" );
		scanf ( " %d", &idir );

		cfl_srch ( filptr, pattern, idir, &iret );

		printf ( "\nCFL_SRCH: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 23 ) {

		printf ( "Enter error number:\n" );
		scanf ( " %d", &ierrno );

		cfl_iret ( ierrno, &iflerr, &iret );

		printf ( "\nCFL_IRET: " );
		if ( iret >= 0 ) printf ( "iflerr = %d, ", iflerr );
		printf ( "iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 24 ) {

		printf ( "Enter the file name:\n" );
		scanf ( " %s", filnam );
		printf ( "Is there a default directory (y/n):\n" );
		scanf ( " %s", ans );
		if  ( ans[0] == 'y' || ans[0] == 'Y' ) {
		    printf ( "Enter the default directory:\n" );
		    defdir = ddname;
		    scanf ( " %s", defdir );
		} else {
		    defdir = NULL;
		}

		cfl_inqr ( filnam, defdir, &lflen, fullpath, &iret );

		if ( iret >= 0 )
		    printf ( "\nCFL_INQR: lflen = %ld, fullpath = %s\n",
			     lflen, fullpath );
		printf ( "CFL_INQR: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 25 ) {

		printf ( "Enter the date/time string:\n" );
		scanf ( " %s", dattim );
		printf ( "Enter the template string:\n" );
		scanf  ( "%s", template );

		cfl_mnam ( dattim, template, fullpath, &iret );

		if ( iret >= 0 )
		    printf ( "\nCFL_MNAM: fullpath = %s\n", fullpath );
		printf ( "CFL_MNAM: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 26 ) {

		cfl_wher ( filptr, &lfaddr, &iret );

		printf ( "\nCFL_WHER: ");
		if ( iret >= 0 ) printf ( "lfaddr = %ld, ", lfaddr );
		printf ( "iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 27 ) {

		printf ( "Enter the file name:\n" );
		scanf ( " %s", fullpath );
		printf ( "Enter the template string:\n" );
		scanf  ( "%s", template );
		printf ( "Enter the default full GEMPAK date/time:\n" );
		scanf  ( "%s", defdat );

		cfl_mdat ( fullpath, template, defdat, dattim, &iret );

		if ( iret >= 0 )
		    printf ( "\nCFL_MDAT: dattim = %s\n", dattim );
		printf ( "CFL_MDAT: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 28 ) {

		printf ( "Enter the directory path name:\n" );
		scanf ( " %s", fullpath );
		printf ( "Enter a template string:\n" );
		scanf ( " %s", template );
		printf ( "Enter sort order: 1=Alpha, -1=Rvrs Alpha\n" );
		scanf ( " %d", &order );

		plen = strlen ( fullpath );
		tlen = strlen ( template );
		maxlen = MXFLSZ * MXNMFL;
		cfl_scnd ( fullpath, &plen, template, &tlen, sep, &maxlen, &order,
					 filstr, &flen, &nf, &iret );
		if ( iret >= 0 )  {
                    if ( nf > 0 ) {
			printf ( "\n Number of files = %d\n", nf );
                        printf ( "%s\n", filstr );
                    } else {
                        printf ( "No files found.\n" );
                    }
		}
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 29 ) {

	        printf ( "Enter type of read (0=all, 1=directory ");
		printf ( "2=directory excluded):\n" );
		scanf ( " %d", &irdtyp );
		printf ( "Enter the directory:\n" );
		scanf ( " %s", fullpath );
		printf ( "Do you have a search string (0=No, 1=Yes):\n" );
		scanf ( " %d", &isrflg );
		if ( isrflg != 0 ) {
		    printf ( "Enter a search string:\n" );
		    scanf ( " %s", search );
		}
		else {
		    search[0] = '\0';
		}
		nf = cfl_rdir ( irdtyp, fullpath, search, &dnlist, &nsdir );
		if ( nf > 0 ) {
		    printf ( "\n Number of entries = %d\n", nf );
		    printf ( "\n Number of subdirectories = %i\n", nsdir );
		    for ( ii = 0; ii < nf; ii++ ) {
		        printf ( "  - %s\n", dnlist[ii]->d_name );
		        free(dnlist[ii]);
		    }
                } else {
                    printf ( "No files found.\n" );
                }
		if (dnlist != NULL ) free(dnlist);
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 30 ) {

		cfl_tbnr ( filptr, &nr, &iret );

		printf ( "\n CFL_TBNR: iret = %d\n\n", iret );
		if ( iret == 0 )
		    printf ( "\n Number of records in table = %d\n", nr );

	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 31 ) {

		printf ( "Enter the table name:\n" );
		scanf ( " %s", tblpath );
		printf ( "Is there a subdirectory (y/n):\n" );
		scanf ( " %s", ans );
		if  ( ans[0] == 'y' || ans[0] == 'Y' ) {
		    printf ( "Enter the subdirectory:\n" );
		    subdir = sdname;
		    scanf ( " %s", subdir );
		} else {
		    subdir = NULL;
		}

		cfl_tinq ( tblpath, subdir, &lflen, fullpath, &iret );

		if ( iret >= 0 )
		    printf ( "\nCFL_TINQ: lflen = %ld, fullpath = %s\n",
			     lflen, fullpath );
		printf ( "CFL_TINQ: iret = %d\n\n", iret );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 32 ) {
		printf ("Enter the file path:\n");
		scanf ( "%s", fullpath );

		printf ( "CFL_ISDIR:  returns %d\n\n", 
					cfl_isdir(fullpath) );
	    }
/*---------------------------------------------------------------------*/
	    else if ( numsub == 33 ) {
		printf ( "Enter the directory path name:\n" );
		scanf ( " %s", fullpath );
		printf ( "Enter a template string:\n" );
		scanf ( " %s", template );
		printf ( "Enter sort order: 1=Alpha, -1=Rvrs Alpha\n" );
		scanf ( " %d", &order );

		cfl_scnt ( fullpath, template, order, &dnlist, &nf, &iret );

		printf ( "\n Number of entries = %d\n", nf );

		if ( nf > 0 ) {
		    for ( ii = 0; ii < nf; ii++ ) {
		        printf ( " - %s\n", dnlist[ii]->d_name );
			free ( dnlist[ii] );
		    }
		} else {
		    printf ( "No files found.\n" );
		}
		if (dnlist != NULL) free ( dnlist );
	    }
/*---------------------------------------------------------------------*/
	}

	if  ( filptr != NULL )  fclose ( filptr );
	return 0;
}
