#include "../../../cgemlib/cvg/cvgcmn.h"

void	vgf_srchlist ( FILE *filptr, char *filnam, long size, 
			int kvgtype, char kgrptyp, int kgrpnum,
			int kactivflg, int kdeletflg, int kstartrec, 
			int kendrec, int numofrec, int *numinsrch, int *iret );
void	vgf_countrec ( FILE *filptr, char *filnam, long size,
			int *cntofrec, int *iret );
void	vgf_getpos ( FILE *filptr, char *filnam, long size, int recno,
			VG_DBStruct *el, int *filpos, int *iret );
void	vgf_typekey ( int typeno, int itype, char typedesc[], int *iret );


int main ( void )
/************************************************************************
 * DUMPVGF								*
 *									*
 * This program reads, searches, and dumps a VGF.			*
 *									*
 * CONTENTS:								*
 *	vgf_srchlist()							*
 *	vgf_countrec()							*
 *	vgf_getpos()							*
 *	vgf_typekey()							*
 **									*
 * Log:									*
 * F. J. Yen/NCEP	11/97		Created	with commands N, F, and	*
 *					H based on testcvg.		*
 * G. Krueger/EAI	 1/98		Truncate too long elements.	*
 * I. Durham/GSC	 5/98		Changed underscore decl. to 	*
 *					an include			*
 * F. J. Yen/NCEP	 9/98		Increase index for help list	*
 * F. J. Yen/NCEP	 5/99		Removed sgi86 printf warning msg*
 * A. Hardy/GSC          5/99		Increased case 'K' indicies     *
 * F. J. Yen/NCEP	10/99		Increased case 'K' index ndx	*
 * F. J. Yen/NCEP 	08/00		Handled older versions		*
 * J. Wu/GSC 		01/01		Casted file pointer for cvg_open*
 * A. Hardy/GSC          1/01           Removed cast pointer for cvgopen*
 * J. Wu/GSC		02/01	        Modified 'unused1' & 'unused2' 	*
 *					to 'smooth' & 'version' 	*
 * E. Safford/GSC	07/01		remove unused vars & updt grps  *
 * S. Schotz/NCEP	11/01		renamed TORN, HAIL, WIND grps	*
 * S. Jacobs/NCEP	 3/03		Inc vg type list from 31 to 34	*
 * m.gamazaychikov/SAIC  3/03		inc ndx check number to 18  	*
 * D.W.Plummer/NCEP	06/03	rm hardwire value for MAX_RECTYPES	*
 * A. Hardy/NCEP	10/03           Inc ndx check 18 -> 20		*
 * T. Lee/SAIC		11/03		used cvgcmn.h			*
 ***********************************************************************/
{
	int		cont, iret, ier;
	int		quitsrch;
	int		flag, level, elevel, ebuff, eflag;
	long		size;

	char		select[LLSCRN], filnam[LLSCRN];
	char		srchcrit[LLSCRN], critval[LLSCRN], chrecnum[LLSCRN];
	int		icritval, ndx;
	char		path[133];
	VG_DBStruct     el;
	FILE		*filptr;
	int		elmnum = 0;
	int		vgstart, orecsz, ounused2;
	char		grp[4];
	char		kgrptyp;
	int		kdeletflg, kactivflg;
	int		kgrpnum, kvgtype, kstartrec, kendrec;
	int		numinsrch, recnum, numofrec;
	int		filposp;
	char		chvgdesc[16], chgrpdesc[16];
	int         mode, istat, iunit, itype;
    	float       xsize, ysize, lllat, lllon, urlat, urlon;
    	float       prjang1, prjang2, prjang3;
    	char        *ptr;
    	char        pro[32], proj[32], dfilnam[128], device[8];

/*---------------------------------------------------------------------*/
	iret = 0;
	size = 0;
	filptr = NULL;
	filnam[0] = '\0';
	cont = G_FALSE;
	eflag = G_FALSE;
        strcpy(grp, "CVG");
	level = 0;
	elevel = 2;
	ebuff = 0;
	kdeletflg = 0;
	kactivflg = 1;
	kgrpnum = 0;
	kgrptyp = 0;
	kvgtype = 0;
	kstartrec = 0;
	kendrec = 0;

	in_bdta(&ier);
	er_stat(&elevel, &ebuff, &eflag, &ier);

	elmnum = 0;
	vgstart = 0;

	while ( cont == G_FALSE ) {
	    printf ( "\n----------------------------------------------"
				"--------------------------------\n" );
            printf ("O = Open VGF                               "
				"? = Print help file\n"
		    "C = Close VGF                              "
				"K = Key to VG and group types\n\n");
            printf ("N = read Next record                       "
				"F = Full dump of record\n"
            	    "R = Read record number                     "
				"H = Hex dump of record\n\n");
	    printf ("S = Set search criteria for L command      "
				"L = List records found in search\n\n");
	    printf ("I = Call GINITP and set generic GAREA, PROJ"
				"                                \n\n");
	    printf ("                           --------------\n"); 
	    printf ( "Select a command or type E for EXIT: " );
	    scanf ( " %s", select );
	    printf ( "\n+++++++++++++++++++++++++++++++++++++++++++++++"
				"+++++++++++++++++++++++++++++++\n" );
	    printf ( "+++++++++++++++++++++++++++++++++++++++++++++++"
				"+++++++++++++++++++++++++++++++\n\n\n" );

	    switch ( select[0] ) {

	        case '?':
		    printf ("                           **HELP FILE**\n\n");
	            printf ("A VG file must be opened with the O command "
			"before it can be processed.\nTo look at another"
			" VGF, the previous file must be closed first"
			" using the\nC command.\n\nTo read "
			"a particular record number, use the R command.  "
			"To read the next\nrecord, use the N command.  "
			"After reading a record, use either the F\ncommand"
			" for a full dump or the H command for a hex "
			"dump of the record read.\n\n");
		    printf ("To search for records that match certain "
			"criteria, use the S command to set\nsearch criteria"
			" such as VG type, group type, and group number.\n"
			"After setting search criteria, use the L command "
			"to list the records found\nthat match the"
			" criteria.  If there is a text string, only"
			" the first 7\ncharacters are printed.\n\n");
		    printf ("The K command provides a key to the "
			"VG type and group type definitions.\n");
		    break;

                case 'c':
                case 'C':

                    cvg_clos ( filptr, &iret );
                    filptr = NULL;
                    elmnum = 0;
                    vgstart = 0;

                    if ( iret == 0 )
                      printf ( "\n---File %s closed.---\n",filnam );
                    else
                      printf ( "\n******NO VG FILE IS OPEN******\n");

                    filnam[0] = '\0';
                    break;

		case 'e':
		case 'E':
	            cont = G_TRUE;
		    break;

		case 'f':
		case 'F':
		    if (filnam[0] == '\0') {
			printf( "\n******OPEN VG FILE AND READ A RECORD "
				"BEFORE ASKING FOR DUMP******\n");
			break;
		    }
		    if (elmnum == 0 ) {
			printf ("\n******READ A RECORD BEFORE DUMPING"
				"******\n");  
			break;
		    }
		    if (elmnum > numofrec) {
		      printf ("\n******END OF FILE******\n");
		    }
		    else {
		      printf (" DUMP OF RECORD %i USING CURRENT VERSION "
			      "OF VGSTRUCT\n\n", elmnum-1);
		      printf (" NOTE:  Value in version will be the current"
			   " version.\n        The actual value in version is"
			   " %d and could be different.\n\n", ounused2);
		      flag = G_FALSE;
		      cvg_dump ( el, el.hdr.recsz, el.hdr.recsz, flag, &iret );
		    }
		    break;

		case 'h':
		case 'H':
		    if (filnam[0] == '\0') {
			printf("\n******OPEN VG FILE AND READ A RECORD "
				"BEFORE ASKING FOR DUMP******\n");
			break;
		    }
		    if (elmnum == 0 ) {
			printf ("\n******READ A RECORD BEFORE DUMPING"
				"******\n");  
			break;
		    }
		    if (elmnum > numofrec) {
		      printf ("\n******END OF FILE******\n");
		    }
		    else {
		      printf (" DUMP OF RECORD %i USING CURRENT VERSION "
			      "OF VGSTRUCT\n\n", elmnum-1);
		      printf (" NOTE:  Value in version will be the current"
			   " version.\n        The actual value in version is"
			   " %d and could be different.\n\n", ounused2);
		      flag = G_TRUE;
		      cvg_dump ( el, el.hdr.recsz, el.hdr.recsz, flag, &iret );
		    }
		    break;

		case 'i':
		case 'I':
		    mode = 1;
                    ginitp ( &mode, &istat, &ier );
                    printf("IRET = %d\n", ier );

                    strcpy ( device, "GN" );

                    iunit = 1;
                    strcpy ( dfilnam, "TESTCVG" );
                    itype = 1;
                    xsize = 500.0F;
                    ysize = 500.0F;

                    printf("GSDEVA... setting:\n");
                    printf("device=%s\n", device );
                    printf("iunit=%d\n", iunit );
                    printf("dfilnam=%s\n", dfilnam );
                    printf("itype=%d\n", itype );
                    printf("x,ysize=%f %f\n", xsize, ysize );

                    gsdeva (device, &iunit, dfilnam, &itype,&xsize,&ysize,&iret,
                        strlen(device), strlen(dfilnam));
                    printf("IRET = %d\n", iret );

		    lllat = 10.0F;
                    lllon = -120.0F;
                    urlat = 50.0F;
                    urlon = -50.0F;
                    strcpy ( proj, "str/90;-105;0" );
                    strcpy ( pro, strtok ( proj, "/" ) );
                    prjang1 = 0.0F;  prjang2 = 0.0F;  prjang3 = 0.0F;
                    ptr = strtok(NULL,";");
                    if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang1 );
                    ptr = strtok(NULL,";");
                    if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang2 );
                    ptr = strtok(NULL,";");
                    if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang3 );

                    printf("GSMPRJ... setting:\n");
                    printf("pro=%s\n", pro );
                    printf("angles=%6.2f %6.2f %6.2f\n", 
			prjang1, prjang2, prjang3 );
                    printf("ll=%6.2f %6.2f\n", lllat, lllon );
                    printf("ur=%6.2f %6.2f\n", urlat, urlon );

                    gsmprj ( pro, &prjang1, &prjang2, &prjang3,
                         &lllat, &lllon, &urlat, &urlon, &iret, strlen(proj));
                    printf("IRET = %d\n", iret );

		    break;

		case 'k':
		case 'K':
		    printf ("              ***KEY TO TYPES***\n\n");
		    printf ("     VG TYPE                    GROUP TYPE\n\n");
		    for (ndx=1; ndx<=MAX_RECTYPES; ndx++) {
	    		vgf_typekey ( ndx, 1, chvgdesc, &iret );
			if (iret != 0) {
			  printf ("******INVALID type category given "
				  "when invoking vgf_typekey******\n");
			  break;
			}
			if (ndx < 20) {
			  vgf_typekey ( ndx, 2, chgrpdesc, &iret );
			  if (iret != 0) {
			    printf ("******INVALID type category given "
			            "when invoking vgf_typekey******\n");
			    break;
			  }
			  printf ("  %2i %-10s                   %2i"
					" %-10s\n",
					ndx, chvgdesc, ndx, chgrpdesc); 
			}
			else {
			  printf ("  %2i %-10s\n",ndx, chvgdesc);
			}
		    }
		    break;

		case 'l':
		case 'L':
		    if (filnam[0] == '\0') {
			printf("\n\n******OPEN VG FILE BEFORE ASKING TO "
				"LIST ELEMENTS******\n\n");
		    }
		    else
		    {
			vgf_srchlist ( filptr, filnam, size, kvgtype,
				kgrptyp, kgrpnum, kdeletflg, kactivflg,
				kstartrec, kendrec, numofrec,
				&numinsrch, &iret );
			if ( iret == -1 ) {
			  printf(" Error in reading record element\n");
			}
			if ( iret == -2 ) {
			  printf(" Error in reading record header\n");
			}
			if ( iret == -3 ) {
			  printf(" Invalid start/end record combination\n");
			}
			else {
			  if ( numinsrch == 1 ) {
			    printf ("\n---Found 1 record");
			  }
			  else {
			    printf ("\n---Found %i records", numinsrch);
			  }
			  printf (" that matched search criteria\n");
			}
		    }
		    break;

		case 'n':
		case 'N':
		    if (filnam[0] == '\0')
		    {
			printf("\n******OPEN VG FILE BEFORE ASKING FOR NEXT"
				" RECORD OR ELEMENT******\n");
		    }
		    else if ((long)vgstart >= size) {

			printf ("\n******AT END OF FILE******\n");
		        if (elmnum <= numofrec) {
		          elmnum++;
	 	          vgstart += orecsz; 
			}
		    }
		    else {
        	        /* read header and element */
			cvg_rdhdr ( filnam, filptr, vgstart, (int)size, &el,
				&flag, &ier);
			orecsz = el.hdr.recsz;
			ounused2 = (int)el.hdr.version;
			cvg_rdele ( &el, vgstart, el.hdr.recsz, filptr, &iret);
			if ( iret != 0 ) {
			  printf ( "\n******ERROR IN READING RECORD******\n");
			}
			else {
		          /* dump header */
		          printf("Record # %i    Starts at %i   Size %i \n",
			 	  elmnum, vgstart, orecsz);
		          printf("  VGFType:  %i    VGFclass: %i   Del? %i \n",
                                  el.hdr.vg_type, el.hdr.vg_class,
				  el.hdr.delete);
	 	          vgstart += orecsz; 
		          elmnum++;
			}
		    }
		    break;

		case 'o':
		case 'O':
                    if ( filptr != NULL ) {
                      printf("\n\n******ONLY ONE FILE AT A TIME CAN BE"
				" OPEN******\n");
                      printf("Please close currently open file before");
                      printf(" attempting to open another.\n");
                    }
                    else {
                      printf ( "Enter the VG file name to open:\n" );
                      scanf ( " %s", filnam );

                      cfl_inqr ( filnam, NULL, &size, path, &ier );
                      cvg_open ( filnam, 2, &filptr, &iret );

 		      if ( iret == 0 ) {
			printf ("---File %s opened.---\n",filnam);
			vgf_countrec ( filptr, filnam, size, &numofrec,
				 &ier );
			printf ("   Number of records = %d\n", numofrec);
                        printf ("   Total size of file = %d bytes\n\n",
                                                             (int)size );
                        elmnum = 0;
                        vgstart = 0;
                      }
		      else {
			printf ("******\n");
                        er_lmsg ( &level, grp, &iret, filnam, &ier,
                                strlen(grp), strlen(filnam) );
			printf ("******\n");
		      }
                    }
		    break;

		case 'r':
		case 'R':
		    if (filnam[0] == '\0') {
			printf("\n******OPEN VG FILE BEFORE READING"
				" RECORD OR ELEMENT******\n");
		    }
		    else {
                        printf ( "Enter the record number to read:\n" );
                        scanf ( "%s", chrecnum );
			recnum = atoi (chrecnum);
			if ( recnum >= numofrec ) {
			  printf ("\n******RECORD IS AT END OF FILE OR "
				"BEYOND******\n");
			  elmnum = numofrec + 1;
	 	          vgstart = (int)size; 
			  break;
			}
			vgf_getpos ( filptr, filnam, size, recnum,
				&el, &filposp, &iret );
			if ( iret == -2 ) {
			  printf("\n******ERROR IN READING HEADER******\n");
			}
			else {
			  ounused2 = (int)el.hdr.version;
			  orecsz = el.hdr.recsz;
			  cvg_rdele (&el, filposp, el.hdr.recsz, filptr,
				&iret );
			  elmnum = recnum + 1;
			  vgstart = filposp + orecsz;
			}
		    }

		    break;

		case 's':
		case 'S':
		    quitsrch = G_FALSE;
	            while ( quitsrch == G_FALSE ) {
		      printf ("\n----------------------------------------"
				"--------------------------------------\n");
		      printf ("Current search criteria settings are:\n"
				" Delete flag = %i       Start record"
				" =%7i       Group type = %i\n"
				" Active flag = %i       End record ="
				"  %7i       group Number = %i\n"
				"                                   "
				"                Vgtype = %i\n",
				kdeletflg, kstartrec, kgrptyp,
				kactivflg, kendrec, kgrpnum, kvgtype);  
		      printf ("                           --------------\n"); 
		      printf ("\nTo set a search criteria, key in the"
				" capital letter of the criteria\n"
				"followed by a space and the new value.\n\n"
				"Let criteria, <Start record> be set to n:\n"
				"  If n>=0, start search at record number n." 
				"  The header record number is 0.\n"
				"  If n<0, start search |n| records"
				" from the last\n"
				"Let criteria, <End record> be set to n:\n"
				"  If n>0, end search at record number n.\n"
				"  If n=0, end search at the last record.\n"
				"  if n<0, end search |n| records from the "
				"last.\n"
				"If criteria <Group type> or <group Number>"
				" or <Vgtype> is set to 0, then\nall "
				"values of that criteria are searched.\n\n"
				"When done setting search criteria, enter Q "
				"to quit out of this mode to\n"
				"return to the main menu and do a List of "
				"the records that match the\nsearch "
				"criteria.\n\n"
				"   Example:  S -10    Search starting 10 "
				"records (elements) from the last\n"
               			"             E 0       till the last "
				"record.\n"
				"   Example:  D 1      Search for deleted "
				"elements (set delete flag).\n"
				"   Example:  D 0      Do not search for "
				"deleted elements (unset delete flag).\n"
				"   Example:  G 2      Search for group "
				"type 2.\n");
		      printf ("                           --------------\n"); 
		      printf ("Enter a criteria setting or Q to QUIT"
				" setting search criteria:\n"); 
		      scanf ( "%s", srchcrit );

		      switch ( srchcrit[0] ) {
			  case 'a':
			  case 'A':
		              scanf ( "%i", &icritval );
			      if ( icritval == 1 ) {
				kactivflg = 1;
			      }
			      else {
				kactivflg = 0;
			      }
			      break;
			  case 'd':
			  case 'D':
		              scanf ( "%i", &icritval );
			      if ( icritval == 1 ) {
				kdeletflg = 1;
			      }
			      else {
				kdeletflg = 0;
			      }
			      break;
			  case 's':
			  case 'S':
		              scanf ( "%i", &kstartrec );
			      break;
			  case 'e':
			  case 'E':
		              scanf ( "%i", &kendrec );
			      break;
			  case 'g':
			  case 'G':
		              scanf ( "%i", &icritval );
			      kgrptyp = (char)icritval;
			      break;
			  case 'n':
			  case 'N':
		              scanf ( "%i", &kgrpnum );
			      break;
			  case 'v':
			  case 'V':
		              scanf ( "%i", &icritval );
			      if (icritval <= 0 )
				icritval = 0;
			      kvgtype = icritval;
			      break;
			  case 'q':
			  case 'Q':
			      quitsrch = G_TRUE;
			      break;
			  default:
		              scanf ( "%s", critval );
			      printf ("\n\n*****ILLEGAL SEARCH CRITERIA."
				      "  Please reenter.******\n\n");
			      break;
		      }
		    }
			  
		    break;

		default:
		    printf ("\n\n*****INVALID COMMAND.  Please reenter."
				"******\n\n");
		    break;

	    }

	}
	return(0);
}

/*=====================================================================*/

void vgf_srchlist ( FILE *filptr, char *filnam, long size, int kvgtype, 
		    char kgrptyp, int kgrpnum, int kdeletflg, 
		    int kactivflg, int kstartrec, int kendrec, 
		    int numofrec, int *numinsrch, int *iret )
/************************************************************************
 * vgf_srchlist								*
 *                                                                      *
 * This function searches for records that meet the search criteria	*
 * and lists them.							*
 *                                                                      *
 * void vgf_srchlist ( filptr, filnam, size, kvgtype, kgrptyp, kgrpnum,	*
 *		kdeletflg, kactivflg, kstartrec, kendrec, numofrec,	*
 *		numinsrch, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *	*filptr		FILE		File pointer			*
 *	*filnam		char		VG File name			*
 *	size		long		Size of file			*
 *	kvgtype		int		Vector Graphics type		*
 *	kgrptyp		char		Group type number		*
 *	kgrpnum		int		Group number			*
 *	kdeletflg	int		Delete flag			*
 *	kactivflg	int		Active flag			*
 *	kstartrec	int		Start record number		*
 *	kendrec		int		End record number		*
 *	numofrec	int		Number of records in VGF	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*numinsrch	int		No. of records found in search	*
 *	*iret		int		Return code			*	
 *					 -1 = error reading VG element	*
 *					 -2 = error reading VG header	*
 *					 -3 = illegal start/end record	*
 *					      combination		*
 **                                                                     *
 * Log:                                                                 *
 * F. J. Yen/NCEP	11/97	Created                                 *
 * F. J. Yen/NCEP	 9/98	Added SPLN5 - SPLN21			*
 * S. Jacobs/NCEP	 5/99	Added TRKSTORM_ELM			*
 * F. J. Yen/NCEP	 8/00	Handled older VGF versions.		*
 * E. Safford/GSC	07/01	remove unused variables			*
 * T. Piper/SAIC	02/04	removed unused variable ounused2	*
 ***********************************************************************/
{
	int		flag, ier, ier1, level, orecsz;
	int		istart, iend, curpos, currecno, heading, ndxvgtyp;
	char		grp[4];
	char		chvgdesc[10];
	VG_DBStruct	el;

/*---------------------------------------------------------------------*/
	ier = 0;
	*numinsrch = 0;
	*iret = 0;
	curpos = 0;
	currecno = 0;
	level = 2;
	heading = G_FALSE;

	if ( kendrec == 0 ) {
	    iend = numofrec - 1;
	}
	else if ( kendrec < 0 ) {
	    iend = kendrec + numofrec - 1;
	}
	else {
	    if ( kendrec >= numofrec ) {
	      printf ("End record number is greater than last, will"
		      " set it to the last record number\n");
	      kendrec = numofrec - 1;
	    }
	    iend = kendrec;
	}

	if ( kstartrec < 0 ) {
	    istart = kstartrec + numofrec - 1;
	}
	else {
	    if ( kstartrec >= numofrec ) {
	      printf ("Start record number is greater than last, will"
		      " set it to the last record number\n");
	      kstartrec = numofrec -1;
	    }
	    istart = kstartrec;
	}

	if ( istart > iend ) {
	    printf (" Starting record number=%i and record=%i!  \n",
		 istart, iend);
	    *iret = -3;
	    return;
	}
	while ( currecno <= iend )
	{
	    /*
	     * Read the VG header.
	     */

	    cvg_rdhdr (filnam, filptr, curpos, (int)size, &el, &flag, &ier);
	    orecsz=el.hdr.recsz;

	    if ( ier != 0 ) {
		printf ( "\n******ERROR IN READING HEADER %i******\n",
			currecno);
		strcpy (grp, "CVG");
		er_lmsg ( &level, grp, &ier, filnam, &ier1,
				strlen(grp), strlen(filnam) );
		*iret = -2;
		return;
	    }

	    /*
	     * Test if matches search criteria
	     */
	    if ( currecno < istart ) {
		curpos += orecsz;
	    	currecno++;
	  	continue;
	    }
	    /*
	     * if kactiflg is 1, then list active elements.
	     * if kdeletflg is 1, then list deleted elements.
	     */
	    if ( kactivflg == 0 ) {
		if ( kdeletflg == 0 ) {
	          curpos += orecsz;
	    	  currecno++;
		  continue;
		}
		else {
		  if ( el.hdr.delete == 0 ) {
	          curpos += orecsz;
                  currecno++;
                  continue;
		  }
		}
	    }
	    else {
		if ( el.hdr.delete == 1 &&  kdeletflg == 0 ) {
	            curpos += orecsz;
                    currecno++;
                    continue;
		}
	    }
	
		    
	    if ( kvgtype != 0 ) {
		if (kvgtype != el.hdr.vg_type) {
	          curpos += orecsz;
	    	  currecno++;
		  continue;
		}
	    }
	    if ( kgrptyp != 0 ) {
		if (kgrptyp != el.hdr.grptyp) {
	          curpos += orecsz;
	    	  currecno++;
		  continue;
		}
	    }
	    if ( kgrpnum != 0 ) {
		if (kgrpnum != el.hdr.grpnum) {
	          curpos += orecsz;
	    	  currecno++;
		  continue;
		}
	    }
	    /*
	     * Found a match; list it.
	     */
	    if ( heading != G_TRUE ) {
	      heading = G_TRUE;
	      printf ("RECORD   POSITION  DELETE      GROUP               "
			"VG_TYPE\n");
	      printf ("  NUM     (BYTE)    FLAG   TYPE (   NUM   )   NUM "
			"(DESCRIPTION)\n\n"); 
	    }

	    ndxvgtyp =  (int)el.hdr.vg_type;
	    
	    vgf_typekey ( ndxvgtyp, 1, chvgdesc, iret );
	    if ( *iret != 0 ) {
	        printf ("******INVALID type category given "
			  "when invoking vgf_typekey******\n");
	        break;
	    }
	    printf ("%6i   %8i     %i     %3i (%9i)   %3i (%s",
		    currecno, curpos, el.hdr.delete, el.hdr.grptyp,
		    el.hdr.grpnum, el.hdr.vg_type, chvgdesc);

	    cvg_rdele (&el, curpos, el.hdr.recsz, filptr, &ier);
	    if ( ier < 0 ) {
		printf ("\n******ERROR IN READING ELEMENT %i******\n",
			currecno);
		strcpy (grp, "CVG");
		er_lmsg ( &level, grp, &ier, filnam, &ier1,
			strlen(grp), strlen(filnam) );
		*iret = -1;
		return;
	    }

	    if ( ndxvgtyp == FRONT_ELM ) {
		printf (",code=%d)\n", el.elem.frt.info.fcode );
	    }
	    else if ( ndxvgtyp == TEXT_ELM || ndxvgtyp == TEXTC_ELM ) {
		printf (",\"%-.7s\")\n", el.elem.txt.text);
	    }
	    else if ( ndxvgtyp == SPTX_ELM ) {
		printf (",\"%-.7s\")\n", el.elem.spt.text);
	    }
	    else if ( ndxvgtyp == SPLN_ELM ) {
		if ( el.elem.spl.info.spltyp == 1 )
		  printf (",Ball & Chn)\n");
		else if (el.elem.spl.info.spltyp == 2 )
		  printf (",Zig Zag)\n");
		else if (el.elem.spl.info.spltyp == 3 )
		  printf (",Scallop)\n");
		else if (el.elem.spl.info.spltyp == 4 )
		  printf (",Arrow)\n");
		else if (el.elem.spl.info.spltyp == 5 )
		  printf (",AltAngTicks)\n");
		else if (el.elem.spl.info.spltyp == 6 )
		  printf (",Fill Arrow)\n");
		else if (el.elem.spl.info.spltyp == 7 )
		  printf (",Box Circles)\n");
		else if (el.elem.spl.info.spltyp == 8 )
		  printf (",Two-Xs)\n");
		else if (el.elem.spl.info.spltyp == 9 )
		  printf (",Fill Circs)\n");
		else if (el.elem.spl.info.spltyp == 10 )
		  printf (",LnFillCircLn)\n");
		else if (el.elem.spl.info.spltyp == 11 )
		  printf (",TickMarks)\n");
		else if (el.elem.spl.info.spltyp == 12 )
		  printf (",Line-X-Line)\n");
		else if (el.elem.spl.info.spltyp == 13 )
		  printf (",Fill Open Box)\n");
		else if (el.elem.spl.info.spltyp == 14 )
		  printf (",FillCirc-X)\n");
		else if (el.elem.spl.info.spltyp == 15 )
		  printf (",Box_X)\n");
		else if (el.elem.spl.info.spltyp == 16 )
		  printf (",LnCircLn)\n");
		else if (el.elem.spl.info.spltyp == 17 )
		  printf (",LnCaretLn)\n");
		else if (el.elem.spl.info.spltyp == 18 )
		  printf (",Ln Caret Ln)\n");
		else if (el.elem.spl.info.spltyp == 19 )
		  printf (",Sine Curve)\n");
		else if (el.elem.spl.info.spltyp == 20 )
		  printf (",Arrow dashed)\n");
		else if (el.elem.spl.info.spltyp == 21 )
		  printf (",Fill Arrow dash)\n");
		else 
		  printf (",Not defined)\n");
	    }
	    else if ( ndxvgtyp == LINE_ELM ) {
		printf (",type=%d)\n", el.elem.lin.info.lintyp);
	    }
	    else if ( ndxvgtyp == TRKSTORM_ELM ) {
		printf (",type=%d)\n", el.elem.trk.info.subtype);
	    }
	    else {
		printf (")\n");
	    }

	    curpos += orecsz;
	    currecno++;
	    (*numinsrch)++;
	}

}

/*=====================================================================*/

void vgf_countrec ( FILE *filptr, char *filnam, long size, 
						int *cntofrec, int *iret )
/************************************************************************
 * vgf_countrec								*
 *                                                                      *
 * This function counts the number of records in the VGF.		*
 *                                                                      *
 * void vgf_countrec ( filptr, filnam, size, cntofrec, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*filptr		FILE		File pointer			*
 *	*filnam		char		VG File name			*
 *	size		long		Size of file			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*cntofrec	int		No. of records found in	VGF	*
 *	*iret		int		Return code			*	
 *					 -1 = error reading VG element	*
 *					 -2 = error reading VG header	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * F. J. Yen/NCEP	11/97	Created                                 *
 * E. Safford		07/01	remove unused variables			*
 ***********************************************************************/
{
	int		flag, ier, ier1, level, curpos;
	char		grp[4];
	VG_DBStruct	el;

/*---------------------------------------------------------------------*/
	ier = 0;
	*iret = 0;
	curpos = 0;
	*cntofrec = 0;
	level = 2;

	while ( (long)curpos < size )
	{
	    /*
	     * Read the VG header.
	     */
	    cvg_rdhdr (filnam, filptr, curpos, (int)size, &el, &flag, &ier);

	    if ( ier != 0 ) {
		strcpy (grp, "CVG");
		printf ( "\n******ERROR READING HEADER******\n");
		printf ("  error return code=%d from cvg_rdhdr"
			",  position=%d\n", ier, curpos );
		er_lmsg ( &level, grp, &ier, filnam, &ier1,
				strlen(grp), strlen(filnam) );
		*iret = -2;
		return;
	    }

	    curpos += el.hdr.recsz;
	    (*cntofrec)++;
	}
}
/*=====================================================================*/

void vgf_getpos ( FILE *filptr, char *filnam, long size, 
		  int recno, VG_DBStruct *el, int *filpos, int *iret )
/************************************************************************
 * vgf_getpos								*
 *                                                                      *
 * This function returns the file position for VGF record number recno.	*
 *                                                                      *
 * void vgf_getpos ( filptr, filnam, size, recno, el, filpos, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*filptr		FILE		File pointer			*
 *	*filnam		char		VG File name			*
 *	size		long		Size of file			*
 *	recno		int		No. of records found in	VGF	*
 *									*
 * Input/Output parameters:						*
 *	*el		VG_DBStruct	VGF element structure of record	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*filpos		int		File position of record		*
 *	*iret		int		Return code			*
 *					 -1 = error reading VG element	*
 *					 -2 = error reading VG header	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * F. J. Yen/NCEP	11/97	Created                                 *
 * E. Safford		07/01	remove unused variables			*
 ***********************************************************************/
{
	int		flag, ier, ier1, level, currecno, nxtfilpos;
	char		grp[4];

/*---------------------------------------------------------------------*/
	ier = 0;
	*iret = 0;
	*filpos = 0;
	nxtfilpos = 0;
	currecno = 0;
	level = 2;

	while ( currecno <= recno )
	{
	    /*
	     * Read the VG header.
	     */
	    cvg_rdhdr (filnam, filptr, nxtfilpos, (int)size, el, &flag, &ier);

	    if ( ier != 0 ) {
		printf ( "\n******ERROR IN READING HEADER******\n");
		printf ( " Error at file position=%d   record number=%d\n",
			*filpos, currecno );
		strcpy (grp, "CVG");
		er_lmsg ( &level, grp, &ier, filnam, &ier1,
				strlen(grp), strlen(filnam) );
		*iret = -2;
		return;
	    }

	    *filpos = nxtfilpos;
	    nxtfilpos += el->hdr.recsz;
	    currecno++;
	}
}

/*=====================================================================*/

void vgf_typekey ( int typeno, int itype, char typedesc[], int *iret )
/************************************************************************
 * vgf_typekey 								*
 *                                                                      *
 * This function gives a description of a VG type or group type number.	*
 *                                                                      *
 * void vgf_typekey ( typeno, itype, typedesc, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	typeno		int		Type number of descrptn desired	*
 *	itype		int		The type category		*
 *					  1 = vg type			*
 *					  2 = group type		*
 *									*
 *                                                                      *
 * Output parameters:                                                   *
 *	typedesc[]	char		Description of type number	*
 *	*iret		int		Return code			*	
 *					  -1 = Invalid itype		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * F. J. Yen/NCEP	11/97	Created                                 *
 * F. J. Yen/NCEP	 5/98	Added DARR_ELM and HASH_ELM             *
 * A. Hardy/GSC  	10/98	Added CMBSY_ELM                         *
 * S. Jacobs/NCEP	 5/99	Added TRKSTORM_ELM			*
 * A. Hardy/GSC          5/99   Added OUTLOOK,LABEL,TROPICL,STNMDL,     *
 *				      MRFSTN				*
 * F. J. Yen/NCEP	10/99	Added Sigmets				*
 * S. Law/GSC		02/00	Added CCF and watch status		*
 * R. Curtis/EAI	10/00	Increased typeno check from > 31 to     *
 *				> 33 to allow for all descriptions      *
 * E. Safford		07/01	remove unused variables & update groups	*
 * S. Jacobs/NCEP	 3/03	Increased typeno from 33 to 34		*
 * m.gamazaychikov/SAIC  3/03	included MESO_DSC to grptypdesc		*
 *				changed number to 18 in typeno check 	*
 * A. Hardy/NCEP	10/03   Added TSTMOLK & JET ELEM;increased typno*
 *                              18 -> 19				*
 * J. Wuy/SAIC		01/04   Added GFA ELEM & increased typno to 20	*
 * B. Yin/SAIC 		02/04   Added TCA ELEM & increased typno to 21	*
 * m.gamazaychikov/SAIC 04/07   Added TCE ELEM, TCT ELEM and TCB ELEM	*
 ***********************************************************************/
{
	char		vgtypdesc[][16] = {"UNDEFINED",
				"LINE",       "FRONT",      "CONTOUR",
				"CIRCL SYM",  "WX SYMBOL",  "WATCH BOX",
				"CENTROID",   "WIND BARB",  "WIND ARRW",
				"CLOUD SYM",  "ICING SYM",  "PRES TEND",
				"PAST WX",    "SKY COVER",  "SP SYM",
				"TURB SYM",   "TEXT",       "JST TEXT",
				"MARKER",     "SP LINE",    "SP TEXT",
				"FILE HDR",   "WIND DARR",  "HASH MARK",
                                "COMBO SYM",  "STRM TRCK",  "SIGMTINTL",
				"SIGMTNCON",  "SIGMTCONV",  "SIGMTOUTL",
				"SIGMTAIRM",  "CONV FCST",  "WTCH STAT",
				"LIST", "VAA VOLCANO", "VAA ASH CLOUD",
				"JET ELEM", "GFA ELEM", "TCA ELEM",
                                "TCE ELEM", "TCT ELEM", "TCB ELEM" };
	char		grptypdesc[][10] = {"UNDEFINED",
				"CLOUD",      "TURB",	    "FRONT",
				"JETS",       "HIGH",	    "LOW",
				"OUTLOOK",    "LABEL",	    "TROPICL",
				"STNMDL",     "MRFSTN",     "HAILOTLK",
                                "TORNOTLK",   "WINDOTLK",   "TOTL_SVR",
                                "FIREOUTL",   "CATG_SVR",   "MESO_DSC",
                                "TSTMOLK"};

/*---------------------------------------------------------------------*/
	*iret = 0;

	if (itype == 1) {
	    /*
	     * itype = 1 is vg type 
	     */
	    if (typeno < 0 || typeno > MAX_RECTYPES ) {
		typeno = 0;
	    }
	    strcpy (typedesc, vgtypdesc[typeno]);
	}
	else if (itype == 2) {
	    /*
	     * itype = 2 is group type
	     */
	     if (typeno < 0 || typeno > 21) {
		typeno = 0;
	    }
	    strcpy (typedesc, grptypdesc[typeno]);
	}
	else {
	    /*
	     * Here for invalid type category
	     */
	    *iret = -1;
	}
	return;

}
