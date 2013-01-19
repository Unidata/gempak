#include "cvgcmn.h"
#include "drwids.h"

int main ( void )
/************************************************************************
 * TESTCVG								*
 *									*
 * This program tests the CGEMLIB "CVG" functions.			*
 *									*
 **									*
 * Log: 								*
 * D. Keiser/GSC	 1/97						*
 * D. Keiser/GSC	 3/97	Added several new types 		*
 * E. Safford/GSC	 3/97	Added tests for types 23 & 24		*
 * D. Keiser/GSC	 4/97	Added type 25 -- special line		*
 * E. Wehner/Eai	 5/97	Added contour fill			*
 * D. Keiser/GSC	 6/97	Added cvg_deall 			*
 * E. Wehner/EAi	 6/97	Added cvg_cp				*
 * E. Wehner/EAi	 6/97	Added scanning capability and		*
 *					group info to all types 	*
 * E. Wehner/EAi	 7/97	Corrected typecasting of gptyp		*
 * E. Wehner/EAi	 7/97	Added fill and closure			*
 * E. Safford/GSC	 7/97	Added type 27  -- special text		*
 * F. J. Yen/NCEP	11/97	Removed unnecessary invocation		*
 *					of cvg_rdrec at beginning.	*
 *					Replaced " " with NULL for	*
 *					default directory in cfl_inqr.	*
 *					Added a help function and	*
 *					grouped commands in instruction *
 *					list.  Made typecasting of	*
 *					gptyp consistent.  Removed	*
 *					contour.			*
 * G. Krueger/EAI	 3/98	Add closed & filled: CVG_SVLIN		*
 * F. J. Yen/NCEP	 5/98	Added DARR_ELM and HASH_ELM		*
 * I. Durham/GSC	 5/98	Changed underscore del. to include	*
 * C. Lin/EAI		 6/98	remove cvg_svctd			*
 * A. Hardy/GSC 	 7/98	Corrected subroutine name typos 	*
 * A. Hardy/GSC 	12/98	Added CIRCLE_ELM			*
 * F. J. Yen/NCEP	 5/99	Corrected scanf format string conversion*
 * S. Jacobs/NCEP	 5/99	Added TRKSTORM_ELM			*
 * G. Krueger/EAI	 5/99	Modified circles for latlon array	*
 * H. Zeng/EAI          08/00   added skip factor for track             *
 * F. J. Yen/NCEP	08/00	Added SIGMETS and handled old vgf format* 
 * M. Li/GSC		10/00	Added itxfn, ithw, sztext to track	*
 * J. Wu/GSC		11/00	Fixed np to 1 for cvg_svsym & cvg_svwnd	*
 * J. Wu/GSC		12/00	Removed all cvg_sv??? except cvg_svfhed	*
 * J. Wu/GSC		01/01	Added cvg_crelm for version conversion	*
 * A. Hardy/GSC          1/01   Removed cast from cvg_open FILE ptr.    *
 * J. Wu/GSC		02/01	Cleanup	& redesigned			*
 * A. Hardy/SAIC        11/01   Added cvg_rdgtn				*
 * T. Lee/SAIC		05/02	Added cvg_valid				*
 * D.W.Plummer/NCEP	06/03	Added option 92 for conv ASCII to VGF	*
 * T. Lee/SAIC		11/03	used cvgcmn.h and read preference table	*
 * R. Tian/SAIC         11/03   Added cvg_snapjet                       *
 * J. Wu/SAIC		01/04	coordinate function 92 with tag2vgf	*
 * J. Wu/SAIC		07/04	add cvg_setfilter & cvg_getfilter	*
 * m.gamazaychikov	08/04	add cvg_rebun				*
 * A. Hardy/NCEp	 4/05   changed numcnty to pointer		*
 * J. Wu/SAIC		06/06	add cvg_matchfilter & cvgrdfilter	*
 * S. Danz/AWC		07/06	New parameter for cvg_writef() function *
 * M. Li/SAIC		03/07	Updated cvg_matchfilter			*
 ***********************************************************************/
{
    int		cont, iret, numsub, num, ier, start, end, loc;
    int		wrtflg=FALSE, orecsz, toerase, gpnum, igptyp, ivgnum;
    int		flag, level, elevel, ebuff, eflag, vgstart, elmnum;
    int		sbtyp, ivgclss;
    long	size;
    char	gptyp, grp[4], string[2], path[133], vgclss;
    char	select[LLSCRN], filnam[LLSCRN], bndnam[25];
    char	ans[LLSCRN], ifname[LLSCRN], ofname[LLSCRN];
    FILE	*filptr;
    VG_DBStruct	el;
    int		mode, istat, iunit, itype, ieloff;
    float	xsize, ysize, lllat, lllon, urlat, urlon;
    float	prjang1, prjang2, prjang3;
    int		ginit = G_FALSE;
    char	*ptr;
    char	pro[32], proj[32], dfilnam[128], device[8], vgnum;
    char	fn[32], vgfn[128], buffer[2048], *str;
    int		nw;
    int         numcnty, cn_fips[30], iexpand, idebug;
    int         npout, imatch;
    float       latout[16], lonout[16];
    FILE	*fp;
    int		filternum;
    char	filterstr[(MAX_FILTER_NUM+1)*DSPLY_FILTER_SZ];
    filter_t	filters[MAX_FILTER_NUM], timeMatched;
    Boolean	filter_match, matchAny;
/*---------------------------------------------------------------------*/
    
    iret = G_NORMAL;
    size = 0;
    filptr = 0;
    level = 0;
    ebuff = 0;
    elevel = 2;
	
    cont = G_FALSE;
    flag = G_FALSE;
    eflag = G_FALSE; 
    end = G_FALSE;

    strcpy(grp, "CVG");
    strcpy(string, " ");

    in_bdta( &ier );
    er_stat( &elevel, &ebuff, &eflag, &ier );

    ctb_pfread ( &ier );
    elmnum = 0;
    vgstart = 0;

    while ( cont == G_FALSE ) {
	numsub = 0;
	
	printf ( "\n\n" );
	printf ("  ? = Print HELP file\n\n");
        printf ("  S = Set VGF    N = Next rec     F = Full dump    H = Hex dump\n\n");
        printf ("  1 = CVG_CRVGF  2 = CVG_OPEN     3 = CVG_CP       4 = CVG_CLOS\n\n");
        printf ("  5 = CVG_RDHDR  6 = CVG_RDELE    7 = CVG_RDREC    8 = CVG_DUMP\n\n"); 
        printf ("  9 = CVG_DELET 10 = CVG_SETGINF 11 = CVG_WRITEF  12 = CVG_RDGTN\n\n" );
        printf (" 13 = CVG_VALID 14 = CVG_SNAPJET\n\n" );
        printf (" 15 = CVG_SETFILTER	16 = CVG_GETFILTER\n\n" );
        printf (" 17 = CVG_REBUN\n\n" );
	printf (" 18 = CVG_RDFILTER  - Read filter.tbl\n\n" );
	printf (" 19 = CVG_MATCHFILTER  - Check if a filter matching or not\n\n" );
        printf (" 90 = GINITP    91 = GSDEVA and GSMPRJ (generic)\n\n" );   
        printf (" 92 = Convert elements from an ASCII file to a VGF file.\n\n" );
 	printf ("\n\n" );
	    
	printf ( "Select a command or function number or type EXIT: " );
	scanf ( " %s", select );
	    
	if ( select[0] == '?' ) {
	    printf ("\n                         *** HELP FILE ***\n\n");
	    printf ("Quick read/dump VGF without knowing start byte ");
	    printf ("& record length:\n\n");
	    printf ("    S = Set VGF name     \t F = Full dump of record\n" );
	    printf ("    N = read Next record \t H = Hex dump of record\n\n" );

	    printf ("Function numbers & descriptions:\n\n");

	    printf ("    1 = cvg_crvgf   - Create a VG file\n" );
	    printf ("    2 = cvg_open    - Open   a VG file\n" );
	    printf ("    3 = cvg_cp      - Copy   a VG file\n" );
	    printf ("    4 = cvg_clos    - Close  a VG file\n" );
	    printf ("    5 = cvg_rdhdr   - Read VG header from opened VGF\n" );
	    printf ("    6 = cvg_rdele   - Read VG elem   from opened VGF\n" );
	    printf ("    7 = cvg_rdrec   - Read VG record from unopened VGF\n" );
	    printf ("    8 = cvg_dump    - Dump VG record from opened VGF\n" );
	    printf ("    9 = cvg_delet   - Delete VG elem from unopened VGF\n" );
	    printf ("   10 = cvg_setginf - Set VG elem's group type & number\n" );
	    printf ("   11 = cvg_writef  - Write VG record to unopened VGF\n" );
	    printf ("   12 = cvg_rdgtn   - Search for a group type and number\n" );
	    printf ("   13 = cvg_valid   - Check validity of a VG file\n" );

	    printf ("   14 = cvg_snapjet - Snap barb, text, hash onto jet line\n" );
	    printf ("   15 = cvg_setfilter  - Set new display filters\n" );
	    printf ("   16 = cvg_getfilter  - Get current display filters\n" );
	    printf ("   17 = cvg_rebun   - Get coordinates of watch encompassing polygon\n" );
	    printf ("   18 = cvg_rdfilter  - Read filter.tbl\n" );
	    printf ("   19 = cvg_matchfilter  - Check if a filter matching or not\n" );

	    printf ( "\n" );
	    printf ( "Select a command or function number or type EXIT: " );
	    scanf ( " %s", select );
	}
	
	switch ( select[0] ) {
	    case 'e':
	    case 'E':
		cont = G_TRUE;
		break;
	    case 'f':
	    case 'F':
		if ( end ) {
		printf ("\n   END OF FILE\n");
		break;
		}
		flag = G_FALSE;
		cvg_dump ( el, el.hdr.recsz, el.hdr.recsz, flag, &iret );
		break;
	    
	    case 's':
	    case 'S':
		printf ( "Enter the VG file name to read:\n" );
		scanf ( " %s", filnam );
		cfl_inqr ( filnam, NULL, &size, path, &ier );
		cvg_open ( filnam, wrtflg, &filptr, &iret );
		printf ( "total size of file = %ld bytes\n\n", size );
		elmnum = 0;
		vgstart = 0;
		break;
	    
	    case 'h':
	    case 'H':
		if ( end ) {
		    printf ("\n   END OF FILE\n");
		    break;
		}
		flag = G_TRUE;
		cvg_dump ( el, el.hdr.recsz, el.hdr.recsz, flag, &iret );
		break;
	    
	    case 'n':
	    case 'N':
		if (strlen(filnam) == (size_t)0)
			printf("Set VGF name before asking for next record \n");
		else {
        	    /* read header and element*/
        	    cvg_rdhdr ( filnam, filptr, vgstart, (int)size, &el,
			        &end, &iret );
		    if ( end ) 
			    printf ("\n   END OF FILE\n");
		    else {
			orecsz = el.hdr.recsz;
			cvg_rdele ( &el, vgstart, el.hdr.recsz,
					filptr, &iret);
			/* dump header */
			printf("Record # %i    Starts at %i   Size %i \n",
				elmnum, vgstart, orecsz);
			printf("  VGFType:  %i    VGFclass: %i   Del? %i\n",
                                el.hdr.vg_type, el.hdr.vg_class, el.hdr.delete);
	 	        vgstart += orecsz; 
		        elmnum++;
		    }
		}
		break;
	    
	    default:
                elmnum = 0;
                vgstart = 0;

		numsub = atoi ( select );
		break;
	}

/*---------------------------------------------------------------------*/
        if ( numsub == 1 ) {

	    printf ( "Enter the VG file name to create:\n" );
	    scanf ( " %s", filnam );
	    cvg_crvgf ( filnam, &iret );

	    printf ( "\nCVG_CRVGF: iret = %d\n\n", iret );
	    er_lmsg ( &level, grp, &iret, filnam, &ier, strlen(grp),
	    strlen(filnam) );
	}

/*---------------------------------------------------------------------*/
	if ( numsub == 2 ) {

	    if ( filptr != NULL ) {
	        printf("\n\nOnly one file at a time can be open!\n");
	        printf("Please close currently open file before");
	        printf(" attempting to open another.\n");
	    }
	    else {
	        printf ( "Enter the VG file name to open:\n" );
	        scanf ( " %s", filnam );
	        printf(" Open for write?: 1 = YES   2 = NO \n");
	        scanf( " %i", &wrtflg);
	        cfl_inqr ( filnam, NULL, &size, path, &ier );
	        cvg_open ( filnam, wrtflg, &filptr, &iret );

	        printf ( "\nCVG_OPEN: iret = %d\n\n", iret );
	        er_lmsg ( &level, grp, &iret, filnam, &ier,
				strlen(grp), strlen(filnam) );
	        if ( iret == 0 )
	        {
	            printf ( "total size of file = %ld bytes\n\n", size );
	            elmnum = 0;
		    vgstart = 0;
		}
	    }
	}
/*---------------------------------------------------------------------*/
	if ( numsub == 3 ) {

	    printf ( "Enter the VG file name to copy to:\n" );
	    scanf ( " %s", filnam );
	    printf ( "Enter the VG file name to copy from:\n" );
	    scanf ( " %s", ifname );
	    printf( "Wipe out %s first? 0 = no, 1=yes\n", filnam);
	    scanf( " %i", &toerase);
	    cvg_cp ( ifname, filnam, toerase, &iret );

	    printf ( "\nCVG_CP: iret = %d\n\n", iret );
	    er_lmsg ( &level, grp, &iret, filnam, &ier,
			strlen(grp), strlen(filnam) );

	}
/*---------------------------------------------------------------------*/
	if ( numsub == 4 ) {

	    cvg_clos ( filptr, &iret );
	    filptr = 0;

	    printf ( "\nCVG_CLOS: iret = %d\n\n", iret );
	    er_lmsg ( &level, grp, &iret, filnam, &ier, strlen(grp),
	    	      strlen(filnam) );
	}
/*---------------------------------------------------------------------*/
	if ( numsub == 5 ) {

	    printf ( "Enter the byte number to start the read:\n" );
	    scanf ( " %i", &start );

	    el.hdr.recsz = 0;
	    cvg_rdhdr ( filnam, filptr, start, (int)size, &el, &end,
			&iret );

	    printf ( "\nCVG_RDHDR: iret = %d\n\n", iret );
	    er_lmsg ( &level, grp, &iret, filnam, &ier, strlen(grp),
		      strlen(filnam) );
	    if ( iret >= 0 ) {
	        printf ("\nVG record size = %d bytes\n\n", el.hdr.recsz );
							
	        orecsz = el.hdr.recsz;
	    }
	}
/*---------------------------------------------------------------------*/
	if ( numsub == 6 ) {

	    orecsz = el.hdr.recsz;
	    cvg_rdele ( &el, start, el.hdr.recsz, filptr, &iret );

	    printf ( "\nCVG_RDELE: iret = %d\n\n", iret );
	    er_lmsg ( &level, grp, &iret, string, &ier, strlen(grp),
		      strlen(string) );
	}
/*---------------------------------------------------------------------*/
	if ( numsub == 7 ) {

	    if ( filptr != NULL ) {
	        printf("\n\nOnly one file at a time can be open!\n");
	        printf("Please close currently open file before");
	        printf(" attempting to open another.\n");
	    }
	    else {
	        printf ( "Enter the VG file name to open:\n" );
	        scanf ( " %s", filnam );
	        printf ( "Enter the byte # to start the read:\n" );
	        scanf ( " %i", &start );

	        cvg_rdrec ( filnam, start, &el, &iret );

	        printf ( "\nCVG_RDREC: iret = %d\n\n", iret );
	        er_lmsg ( &level, grp, &iret, filnam, &ier,
	        	  strlen(grp), strlen(filnam) );
	        /*
	         * Invocation of cvg_rdrec tests that routine.
	         * But cvg_rdhdr is called to set orecsz even
	         * though cvg_rdrec calls cvg_rdhdr.
	         */
	        cvg_rdhdr (filnam, filptr, start, (int)size, &el, &end, &iret);
		orecsz = el.hdr.recsz;
	    }
	}
/*---------------------------------------------------------------------*/
	if ( numsub == 8 ) {

	    printf ( "Dump actual contents of element?(y/n)\n");
	    scanf ( " %s", ans );
	    if  ( ans[0] == 'y' || ans[0] == 'Y' ) {
	        printf ( "Enter # of bytes to dump:\n" );
	        scanf ( " %d", &num );
	        flag = G_TRUE;
	    }
	    else {
	        num = el.hdr.recsz; 
	        flag = G_FALSE;
	    }

	    cvg_dump ( el, el.hdr.recsz, num, flag, &iret );

	    printf ( "\nCVG_DUMP: iret = %d\n\n", iret );
	    er_lmsg ( &level, grp, &iret, string, &ier, strlen(grp),
	    	      strlen(string) );	    
	}
/*---------------------------------------------------------------------*/
	if ( numsub == 9 ) {

	    if ( filptr != NULL ) {
	        printf("\n\nOnly one file at a time can be open!\n");
	        printf("Please close currently open file before");
	        printf(" attempting to open another.\n");
	    }
	    else {
	        printf ( "Enter the name of the VG file to open " );
	        printf ( "and mark the deletion flag:\n" );
	        scanf ( " %s", filnam );
	        printf ( "Enter the byte # to mark as deleted:\n" );
	        scanf ( " %i", &start );

	        cvg_delet ( filnam, start, FALSE, &iret );

	        printf ( "\nCVG_DELET: iret = %d\n\n", iret );
	        er_lmsg ( &level, grp, &iret, filnam, &ier,
	    	          strlen(grp), strlen(filnam) );
	    }
	}
/*---------------------------------------------------------------------*/
        if ( numsub == 10 ) {

            printf ("Enter the VG file name to change group on:\n");
            scanf ( " %s", filnam );
	    printf ("Enter offset to element to edit \n");
	    scanf( " %i", &vgstart);
	    printf("Enter group type:\n");
	    scanf( " %i", &igptyp);
	    gptyp = (char)igptyp;
	    printf("Enter group number: \n");
	    scanf( " %i", &gpnum);
            cvg_setginf ( filnam, vgstart, gptyp, gpnum, &iret );

            printf ( "\nCVG_SETGINF: iret = %d\n\n", iret );
            er_lmsg ( &level, grp, &iret, filnam, &ier,
                      strlen(grp), strlen(filnam) );
        }
/*---------------------------------------------------------------------*/
	if ( numsub == 11 ) {

	    if ( filptr != NULL ) {
	        printf("\n\nOnly one file at a time can be open!\n");
	        printf("Please close currently open file before");
	        printf(" attempting to open another.\n");
	    }
	    else {                    
	        printf ("\nEnter the VG file to open and write the " );
	        printf ( "element to:\n" );
	        scanf ( " %s", filnam );
		    
	        cvg_crelm( filnam, &iret );  /* calles CVG_WRITEF */
		    
	        printf ( "\nCVG_WRITEF: iret = %d\n\n", iret );
	        er_lmsg ( &level, grp, &iret, filnam, &ier,
	    		  strlen(grp), strlen(filnam) );
	        if ( iret >= 0 ) 
	            printf ("\nThe VG record was created successfully\n");	    
	    }
        }
/*---------------------------------------------------------------------*/
     	if ( numsub == 12 ) {

            if ( filptr == NULL ) {
	        printf("\n Please open a VG file.\n ");
            }
	    else {

	    	printf ("Enter the starting offset location: \n");
	    	scanf( " %i", &vgstart);
	    	printf("Enter Group Type number:\n");
	    	scanf( " %i", &igptyp);
	    	gptyp = (char)igptyp;
	    	printf("Enter VG Group Class number:\n");
	    	scanf( " %i", &ivgclss);
	    	vgclss = (char)ivgclss;
	    	printf("Enter VG Group Type number:\n");
	    	scanf( " %i", &ivgnum);
	    	vgnum = (char)ivgnum;
	    	printf("Enter VG Subtype number:\n");
	    	scanf( " %i", &sbtyp);
            	cvg_rdgtn ( filnam, filptr, &size, vgstart, gptyp, vgclss,
	                    vgnum, sbtyp, &ieloff, &gpnum, &iret );

		printf (" \nElement offset : %d   Group number : %d \n",
	                  ieloff, gpnum );
		printf ( "\nCVG_RDGTN: iret = %d\n\n", iret );
		er_lmsg ( &level, grp, &iret, filnam, &ier,
                      strlen(grp), strlen(filnam) );
	    }
    	}
/*---------------------------------------------------------------------*/
     	if ( numsub == 13 ) {
	    printf ( "Enter the VG file name to validate:\n" );
	    scanf ( " %s", filnam );
	    cvg_valid ( filnam, &iret );
	    printf ( "\nCVG_VALID: iret = %d\n\n", iret );
	    er_lmsg ( &level, grp, &iret, filnam, &ier, strlen(grp), 
		  strlen(filnam) );
	    if ( iret == 0 ) {
		cfl_inqr ( filnam, NULL, &size, path, &ier );
	    	printf ( "total size of file = %ld bytes\n\n", size );
	    }
	}
/*---------------------------------------------------------------------*/
     	if ( numsub == 14 ) {
            printf ( "Enter VGF filename contains jet element:\n");
            scanf ( " %s", ifname );
            printf ( "Enter an output file name:\n");
            scanf ( " %s", ofname );
            cfl_inqr ( ifname, NULL, &size, path, &ier );
            if ( ier != 0 || size == 0 ) {
                printf ( "Has to input a valid VGF file.\n" );
                continue;
            }
            cvg_open ( ifname, G_FALSE, &fp, &ier );
            start = 0;
            while ( 1 ) {
                cvg_rdhdr ( ifname, fp, start, (int) size, &el, &flag, &ier );
                if ( flag ) {
                    break;
                }
                cvg_rdele ( &el, start, el.hdr.recsz, fp, &ier );
                if ( el.hdr.vg_class == CLASS_MET &&
                     el.hdr.vg_type == JET_ELM ) {
                    cvg_snapjet ( &el, &el, &ier );
                }
                cvg_writef( &el, -1, el.hdr.recsz, ofname, FALSE, &loc, &ier );
                start += el.hdr.recsz;
            }
            cvg_clos ( fp, &ier );
	}
/*---------------------------------------------------------------------*/
     	if ( numsub == 15 ) {
	    printf ( "Enter the new filter string:\n" );
	    scanf ( " %s", filterstr );
	    cvg_setfilter ( filterstr, &iret);
	    printf ( "\nCVG_SETFILTER: IRET = %d\n\n", iret );
	}
/*---------------------------------------------------------------------*/
     	if ( numsub == 16 ) {
	    cvg_getfilter ( &filternum, filters, &iret );
	    printf ( "\nCVG_GETFILTER: IRET = %d\n\n", iret );
	    if ( iret == 0 ) {
	        printf ( "\n Number of filters: %d\n", filternum );
	    	for ( start = 0; start < filternum; start++ ) {
		    printf ( "Filter[%d] = <%s>\n", start, filters[start] );
	        }
	    }
	}
/*---------------------------------------------------------------------*/
        if ( numsub == 17 ) {
            mode = 1;
            ginitp ( &mode, &istat, &ier );
            strcpy ( device, "GN" );
            iunit = 1;
            strcpy ( dfilnam, "REBUNDLE" );
            itype = 1;
            xsize = 500.0F;
            ysize = 500.0F;
            gsdeva ( device, &iunit, dfilnam, &itype, &xsize, &ysize, &ier,
                     strlen(device), strlen(dfilnam));
            lllat = 0.0F;
            lllon = -150.0F;
            urlat = 80.0F;
            urlon = 40.0F;
            strcpy ( pro, "str" );
            prjang1 = 90.0F;  prjang2 = -105.0F;  prjang3 = 0.0F;
            gsmprj ( pro, &prjang1, &prjang2, &prjang3,
                    &lllat, &lllon, &urlat, &urlon, &ier, strlen(pro));

	    printf ( "Enter the number of county FIPS codes:\n" );
	    scanf ( " %i", &numcnty );
	    if ( numcnty > 0 ) {
	        printf ( "Enter the FIPS codes:\n" );
	        for ( start = 0; start < numcnty; start++ ) {
	             scanf ( " %i", &cn_fips[start] );
	        }
	        printf ( "Enter the expansion flag (0-FALSE, 1-TRUE):\n" );
	        scanf ( " %i", &iexpand );
	        printf ( "Enter the debug     flag (0-FALSE, 1-TRUE):\n" );
	        scanf ( " %i", &idebug );
	        printf ( "Enter the bounds file name:\n" );
	        scanf ( " %s", bndnam );
                cvg_rebun ( &numcnty, cn_fips, &iexpand, &idebug, bndnam,
                           &npout, latout, lonout, &iret );
                printf ( "\nCVG_REBUN: IRET = %d\n\n", iret );
                if ( iret == 0 ) {
                    printf ( "\n Number of points in the line: %d\n", npout );
                    for ( start = 0; start < npout; start++ ) {
                        printf ( "Lat[%d] = %f  Lon[%d] = %f\n", start, 
                                 latout[start], start, lonout[start] );
                    }
                }
            }
            else {
                printf ( "number of county FIPS codes must be greater than 0.\n" );
            }
             
        }
/*---------------------------------------------------------------------*/
     	if ( numsub == 18 ) {
	    cvg_rdfilter ( &iret );
	    cvg_gettblfilter ( &filternum, filters, &iret );
	    printf ( "\nCVG_RDFILTER: IRET = %d\n\n", iret );
	    if ( iret == 0 ) {
	        printf ( "\n Number of filters in filter.tbl: %d\n", filternum );
	    	for ( start = 0; start < filternum; start++ ) {
		    printf ( "Filter[%d] = <%s>\n", start, filters[start] );
	        }
	    }
	}
/*---------------------------------------------------------------------*/
     	if ( numsub == 19 ) {
	    cvg_rdfilter ( &ier );
	    cvg_gettblfilter ( &filternum, filters, &iret );
	    printf ( "Select filters in the table:\n");
	    for ( start = 0; start < filternum; start++ ) {
		printf ( "%s", filters[start] );
	        if ( start < (filternum - 1) ) 
                   printf ( ";"); 
	    }
	    printf ( "\n");
	    
	    scanf ( " %s", filterstr );
	    cvg_setfilter ( filterstr, &iret);
	    
	    printf ( "Enter the filter string to be matched:\n" );
	    scanf ( " %s", filterstr );
	    printf ( "Match to any available time? 1 = yes, 0 = no\n" );
 	    scanf ( "%d", &imatch );
	    matchAny = (imatch == 1) ? True : False;
	    cvg_matchfilter ( filterstr, matchAny, &filter_match, timeMatched, &iret);
	    printf ( "\nCVG_MATCHFILTER: IRET = %d\n\n", iret );
            if ( filter_match ) {
	        printf ( "Filter [%s] is matched\n", filterstr  );
		if ( matchAny) printf ("Actual matched time:%s\n", timeMatched);
	    }
	    else {
	        printf ( "Filter [%s] is not matched\n", filterstr);	    
	    }
	}
/*---------------------------------------------------------------------*/
            if ( numsub == 90 ) {

                mode = 1;
                ginitp ( &mode, &istat, &ier );
		printf("IRET = %d\n", ier );

                ginit = G_TRUE;

            }
/*---------------------------------------------------------------------*/

            if ( numsub == 91 ) {

              if ( ginit == G_FALSE )  {
                printf("Must run GINITP first.\n" );
              }
              else  {
		/*
                printf ( "Enter DEVICE (XW, GN, etc.):\n" );
                scanf ( " %s", device );
		*/
                strcpy ( device, "GN" );

                iunit = 1;
                strcpy ( dfilnam, "TESTCVG" );
                itype = 1;
                xsize = 500.F;
                ysize = 500.F;

		printf("GSDEVA... setting:\n");
		printf("device=%s\n", device );
		printf("iunit=%d\n", iunit );
		printf("dfilnam=%s\n", dfilnam );
		printf("itype=%d\n", itype );
		printf("x,ysize=%f %f\n", xsize, ysize );

                gsdeva (device, &iunit, dfilnam, &itype, &xsize, &ysize, &iret,
                        strlen(device), strlen(dfilnam));
		printf("IRET = %d\n", iret );

		/*
                printf ( "Enter GAREA in form latll;lonll;latur;lonur :\n" );
                scanf ( " %f;%f;%f;%f", &lllat, &lllon, &urlat, &urlon );
                printf ( "Enter PROJ in form pro/ang1;ang2;ang3 :\n" );
                scanf ( " %s", proj );
                strcpy ( pro, strtok ( proj, "/" ) );
                prjang1 = 0;  prjang2 = 0;  prjang3 = 0;
                ptr = strtok(NULL,";");
                if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang1 );
                ptr = strtok(NULL,";");
                if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang2 );
                ptr = strtok(NULL,";");
                if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang3 );
		*/

                lllat = 10.F;
                lllon = -120.F;
                urlat = 50.F;
                urlon = -50.F;
                strcpy ( proj, "str/90;-105;0" );
                strcpy ( pro, strtok ( proj, "/" ) );
                prjang1 = 0.F;  prjang2 = 0.F;  prjang3 = 0.F;
                ptr = strtok(NULL,";");
                if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang1 );
                ptr = strtok(NULL,";");
                if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang2 );
                ptr = strtok(NULL,";");
                if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang3 );

		printf("GSMPRJ... setting:\n");
		printf("pro=%s\n", pro );
		printf("angles=%6.2f %6.2f %6.2f\n", prjang1, prjang2, prjang3 );
		printf("ll=%6.2f %6.2f\n", lllat, lllon );
		printf("ur=%6.2f %6.2f\n", urlat, urlon );

                gsmprj ( pro, &prjang1, &prjang2, &prjang3,
                         &lllat, &lllon, &urlat, &urlon, &iret, strlen(proj));
		printf("IRET = %d\n", iret );

              }
            }

/*---------------------------------------------------------------------*/
            if ( numsub == 92 ) {

		printf ( "Enter the ASCII file name to read elements from :\n" );
		scanf ( " %s", fn );
		printf ( "Enter the VGF file name to write elements to :\n" );
		scanf ( " %s", vgfn );

    		fp = cfl_ropn ( fn, "", &ier );
		nw = 0;
    		while ( ier == 0 )  {

		    str = fgets ( buffer, sizeof(buffer), fp );

		    if ( str != (char *)NULL )  {
	                if ( buffer[0] != '!' )  {
                	    cvg_t2v ( buffer, &el, &ier );
			      if ( ier == 0 ) {
			        if ( el.hdr.vg_type != FILEHEAD_ELM )  {
                                  cvg_writef ( &el, -1, el.hdr.recsz, vgfn, FALSE, &loc, &ier );
			          if ( ier != 0 )  {
			          printf("ERROR writing element to file %s\n", vgfn );
			        }
			        else  {
			          nw++;
			        }
			      }
			    }
			    else {
		              printf("ERROR processing buffer=\"%s\"\n", buffer );			    
			    }
			}
		    }
		    else  {
			ier = -1;
		    }
		}
		printf("Number of elements decoded and written successfully = %d\n", nw );

            }

/*---------------------------------------------------------------------*/
/*      Add more functions here......                                  */
/*---------------------------------------------------------------------*/

    }  /* End of while loop */

    return ( 0 );

}
