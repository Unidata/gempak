#include "crgcmn.h"
#include "drwids.h"
#include "vgstruct.h"

int main ( void )
/************************************************************************
 * TESTCRG								*
 *									*
 * This program tests the CGEMLIB "CRG" functions.			*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 8/97	Created					*
 * F.J.Yen/NCEP          2/98   Cleaned up.  Added commands 8 and 9.	*
 * C. Lin/EAI            4/98   Added new group commands: 11->16	*
 * D.W.Plummer/NCEP	 4/98	Added new type commands: 17, 18		*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * A. Hardy/GSC          1/01   Removed cast from FILE pointer 		*
 * J. Wu/SAIC           12/01   add/modify layer-related test commands	*
 * E. Safford/SAIC	02/02	add crg_mvallayer			*
 * J. Wu/SAIC           03/02   add parameter to crg_mvallayer		*
 * M. Li/SAIC		04/02   add crg_ggnhl				*
 * E. Safford/SAIC	04/02	param change for crg_clearLayer		*
 * T. Lee/SAIC          11/03   changed WORK_FILE to FILE_NAME		*
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * E. Safford/SAIC	10/04	add crg_goffsets			*
 * m.gamazaychikov/SAIC	12/05	add crg_build				*
 * J. Wu/SAIC		07/07	add functions to handle second record	*
 * J. Wu/SAIC		08/04	add crg_getsecrange			*
 ***********************************************************************/
{
	int		joffset, elnum, *inxarry, layer;
	int		cont, numsub, i, ier, iret;
	int		level, elevel, ebuff, eflag;
	int		index, ityp, grpnum, nexp, nelm;
	int		high_grpnum, low_grpnum;
	char		grptyp, select[LLSCRN], filnam[LLSCRN], reb[8];
	char		grp[8], path[133], vg_class, vg_type;
	FILE		*filptr;
	char		logstr[10];
	float		urx, ury, llx, lly, rl, rr, rt, rb;
	int		initflg=0;
	int		maxedel, setiret, ndx, inxstart, inxend;
	long		size;
	int		iunit, mode, istat, itype;
	float		xsize, ysize;
	float		lllat, lllon, urlat, urlon;
	float		prjang1, prjang2, prjang3;
	char		device[3], dfilnam[9], proj[4];
        int             endret, ieop=1;
	VG_DBStruct	el;
	Boolean		moved;
	int		ii, num_off, offsets[ MAX_EDITABLE_ELEMS ];
	int		assocrec;
/*---------------------------------------------------------------------*/

    iret = 0;
    filptr = 0;
    cont = G_FALSE;
    eflag = G_FALSE;
    strcpy(grp, "CRG");
    level = 0;
    elevel = 2;
    ebuff = 0;
    layer = 0;
    
    in_bdta(&ier);
    er_stat(&elevel, &ebuff, &eflag, &ier);

    ctb_pfread ( &ier );


    while ( cont == G_FALSE ) 
    {
	numsub = 0;
	printf ("\n----------------------------------------------------------"
		"--\n\n");
	printf ("  1 = CRG_INIT          2 = CRG_CLEAR       3 = CRG_GETINX\n");
	printf ("  4 = CRG_CLROFFST      5 = CRG_SAVE        6 = CRG_NEWINX\n");
	printf ("  7 = CRG_GOFFSET \n");
	printf (" 11 = CRG_SGRP         12 = CRG_GGRP\n");
	printf (" 13 = CRG_GGNEL        14 = CRG_GGINX      15 = CRG_GGBND\n");
	printf (" 16 = CRG_GGNXT        17 = CRG_STYP       18 = CRG_GTYP\n");
	printf (" 19 = CRG_SETLAYER     20 = CRG_GETLAYER   21 = CRG_CLEARLAYER\n");
	printf (" 22 = CRG_MVALLAYER    23 = CRG_GGNHL      24 = CRG_GOFFSETS\n");
	printf (" 25 = CRG_BUILD        26 = CRG_SAREC      27 = CRG_GASSOCREC\n");
	printf (" 28 = CRG_ISAUXREC     29 = CRG_SETAUXREC  30 = CRG_LKATTR\n");
	printf (" 31 = CRG_GETSECRANGE\n");
	printf ("  8 - Set range from VGF (crg_getinx, crg_mkRange, crg_set, crg_rebuild)\n");
	printf ("  9 - List range by index (crg_goffset)\n");
	printf ("                    ---------------------\n\n");
	printf ("Select a function number or type EXIT: " );
	scanf ( " %s", select );

	if (select[0] == 'e' || select[0] =='E') {
		cont = G_TRUE;
		if (initflg == 2) {
        	    strcpy(grp, "GEMPLT");
            	    gendp ( &ieop, &endret );
                    er_lmsg ( &level, grp, &endret, NULL, &ier, strlen(grp) );
		    break;
		}
	}
	else {
		numsub = atoi ( select );

/*---------------------------------------------------------------------*/
	  switch (numsub)
	  {
/*
 *	----------------------------------------------------------
 *	       CRG_INIT - Initialize empty settings
 *	----------------------------------------------------------
 */
	    case 1:

		crg_init( &iret);
		printf ( "\nCRG_INIT: iret = %d\n\n", iret );

		if ((iret == 0) & (initflg == 0)) {
		  initflg = 1;
		}
		er_lmsg ( &level, grp, &iret, filnam, &ier, strlen(grp),
		  strlen(filnam) );
    		break;

	    case 2:

                printf ( "Enter the element number to clear:\n" );
                scanf ( " %i", &elnum );

		crg_clear(elnum, &iret);
		printf ( "\nCRG_CLEAR: iret = %d\n\n", iret );
		er_lmsg ( &level, grp, &iret, filnam, &ier, strlen(grp),
		  strlen(filnam) );
    		break;
	 
	    case 3:

	  	printf("Enter the file position of the element to find \n");
                scanf ( " %i", &joffset );

		crg_getinx(joffset, &index, &iret);
                printf ( "\nCRG_GETINX: iret = %d\n\n", iret );
		sprintf(logstr, "%i ", joffset);
		er_lmsg ( &level, grp, &iret, logstr, &ier, strlen(grp),
		  strlen(logstr) );

		printf("Element at file pos %i is at display pos %i \n",
				joffset, index);
		break;

	    case 4:

	  	printf("Enter the file position of the element to clear \n");
                scanf ( " %i", &joffset );

		crg_clroffst( joffset, &iret);
                printf ( "\nCRG_CLROFFST: iret = %d\n\n", iret );
		sprintf(logstr, "%i ", joffset);
		er_lmsg ( &level, grp, &iret, logstr, &ier, strlen(grp),
		  strlen(logstr) );
		break;

	    case 5:

		printf("Enter file position of element to save range on: \n");
		scanf(" %i", &joffset);
		printf("Enter displayed element number to save range on: \n");
		scanf(" %i", &elnum);
		printf("Enter URX \n");
		scanf(" %f", &urx);
		printf("Enter URY \n");
		scanf(" %f", &ury);
		printf("Enter LLX \n");
		scanf(" %f", &llx);
		printf("Enter LLY \n");
		scanf(" %f", &lly);

		crg_save( elnum, joffset, llx, lly, urx, ury, &iret );
                printf ( "\nCRG_SAVE: iret = %d\n\n", iret );
		sprintf(logstr, "%i ", joffset);
		er_lmsg ( &level, grp, &iret, logstr, &ier, strlen(grp),
		  strlen(logstr) );
		break;

	    case 6:

		crg_newinx( &elnum, &iret);
                printf ( "\nCRG_NEWINX: iret = %d\n\n", iret );
		er_lmsg ( &level, grp, &iret, NULL, &ier, strlen(grp) );
		printf("First available slot is at %i \n", elnum);
		break;

	    case 7:

		printf("Enter slot to retrieve from: \n");
	  	scanf(" %i", &elnum);

		crg_goffset(elnum, &joffset, &ier);
                printf ( "\nCRG_GOFFSET: iret = %d\n\n", iret );
		er_lmsg ( &level, grp, &iret, NULL, &ier, strlen(grp) );
		printf("file position for element in slot %i is %i \n",
				elnum, joffset);
		break;

	    case 8:

                if (initflg == 0) {
                    printf ("\n*****CRG_INIT MUST BE INVOKED FIRST*****\n");
                    break;
                }
                if ( filptr != NULL ) {
                  cvg_clos ( filptr, &iret );
                  filptr = NULL;
                  if ( iret == 0 ) {
                    printf ( "\n---File %s closed.---\n",filnam );
		  }
                }

		printf ( "Enter the VG file name to open:\n" );
		scanf ( " %s", filnam );
		cfl_inqr ( filnam, NULL, &size, path, &ier );
		cvg_open ( filnam, 2, &filptr, &iret );
		if ( iret == 0 ) {
		    printf ("---File %s opened.---\n",filnam); 
                }
                else {
                    printf ("******\n");
                    strcpy(grp, "CVG");
                    er_lmsg ( &level, grp, &iret, filnam, &ier,
                             strlen(grp), strlen(filnam) );
                    printf ("******\n");
                }

		printf ("Enter space delimited map boundary coordinates (LLlat LLlon URlat URlon):\n");
		scanf ("%f %f %f %f", &lllat, &lllon, &urlat, &urlon);
		/*
		* Initialize GPLT.
		*/
        	strcpy(grp, "GEMPLT");
       		mode = 1;
	 	iret = 0;
        	ginitp ( &mode, &istat, &iret );
        	if ( iret != 0 ) {
            	    er_lmsg ( &level, grp, &iret, NULL, &ier, strlen(grp) );
            	    break;
        	}
		initflg = 2;

        	iunit = 1;
        	strcpy (device, "XW");
        	strcpy (dfilnam, "TESTCRG");
        	itype = 1;
		xsize = 500.F;
		ysize = 500.F;

        	gsdeva (device, &iunit, dfilnam, &itype, &xsize, &ysize, &iret,
                	strlen(device), strlen(dfilnam));
        	if ( iret != 0 ) {
            	    er_lmsg ( &level, grp, &iret, NULL, &ier, strlen(grp) );
            	    break;
        	}

        	prjang1 = 0.F;
        	prjang2 = 0.F;
        	prjang3 = 0.F;
        	strcpy (proj, "CED");

        	gsmprj (proj, &prjang1, &prjang2, &prjang3, &lllat, &lllon,
                    &urlat, &urlon, &iret, strlen(proj));

        	if ( iret != 0 ) {
            	    er_lmsg ( &level, grp, &iret, NULL, &ier, strlen(grp) );
		    break;
        	}

                setiret = 0;
                joffset = 0;
                while (joffset < size) {
                    cvg_rdrec (filnam, joffset, &el, &iret);
                    if ( iret != 0 ) {
                      printf ("\n******ERROR IN READING RECORD AT %i******\n",
                                joffset);
                      break;
                    }
                    crg_getinx (joffset, &elnum, &iret);

                    printf ( "\nCRG_GETINX: iret = %d\n", iret );
                    sprintf(logstr, "%i ", joffset);
                    strcpy(grp, "CRG");
                    er_lmsg ( &level, grp, &iret, logstr, &ier, strlen(grp) );

                    /* 
		     *  crg_mkRange is called internally in crg_set/crg_rebuild.
		     */ 
		    crg_set (&el, joffset, layer, &iret);
                    if (iret == 0 ) {
                      printf ("Range record for file position %i is set\n",
                                joffset);
                    }
                    else {
                      printf ( "\nCRG_SET: iret = %d\n", iret );
                      strcpy(grp, "CRG");
                      er_lmsg ( &level, grp, &iret, NULL, &ier, strlen(grp) );
                      setiret++;
                      break;
                    }

                    joffset += el.hdr.recsz;

                }
                
		if (setiret == 0 ) {
                    printf ("\nRange record is set from file %s\n", filnam);
		}
                else {
                    printf ("\nErrors occurred in setting range record\n");
                }
                
		printf ("\nRebuild the range record from WORK_FILE(y/n)?: \n");
	        scanf ( " %s", reb );

	        if (reb[0] == 'Y' || reb[0] =='y') {
		    cfl_inqr ( cvg_getworkfile(), NULL, &size, path, &ier );
		    
		    if ( ier != 0 ) {
			cvg_cp( filnam, cvg_getworkfile(), 1, &ier );
		    }			
		    
		    crg_rebuild( ); 
		    printf ("\nThe range record has been rebuilt.\n"); 
		}		    

		break;

            case 9:

                if (initflg == 0) {
                    printf ("\n*****CRG_INIT MUST BE INVOKED FIRST*****\n");
                    break;
                }
                if (filnam[0] == '\0') {
                    printf ("\n*****OPEN VGF BEFORE LISTING RANGE RECORD\n");
                    break;
                }

                printf ( "Enter the first index number to list:\n" );
                scanf ( "%i", &inxstart );
                if ( (inxstart < 0) | (inxstart > MAX_EDITABLE_ELEMS) ) {
		    maxedel = MAX_EDITABLE_ELEMS;
                    printf ("First index number must be >= 0 and <= %i\n",
                                maxedel);
                    break;
                }
                printf ( "Enter the last index number to list:\n" );
                scanf ( "%i", &inxend );
                if ( (inxend < inxstart) | (inxend > MAX_EDITABLE_ELEMS) ) {
                    printf ("Last index must be >= starting index and <= %i\n",
                            maxedel);
                    break;
                }
                printf (" INDEX   OFFSET  GTYP(GNUM) LAYER   LLX       LLY       "
			"URX       URY\n");

                for (ndx=inxstart; ndx<=inxend; ndx++) {
                    crg_goffset (ndx, &joffset, &iret);

                    if ( iret !=0 ) {
                      strcpy(grp, "CRG");
                      er_lmsg ( &level, grp, &iret, NULL, &iret, strlen(grp) );
                    }
 
                    printf(" %4i  %8i    %d  (%d)     %d %9.3f %9.3f %9.3f %9.3f\n", 
			   ndx, joffset, range[ndx].grptyp, range[ndx].grpnum,
			   range[ndx].layer,
                           range[ndx].rleft, range[ndx].rbottom, 
			   range[ndx].rright, range[ndx].rtop);
                }
                break;

	    case 11:	/* CRG_SGRP */

		printf("Enter the element number: \n");
	  	scanf(" %i", &elnum);

		printf("Enter the group type and group number: \n");
	  	scanf(" %i %i", &ityp, &grpnum);
		grptyp = ityp;

		crg_sgrp(elnum, grptyp, grpnum, &iret);
                printf ( "\nCRG_SGRP: iret = %d\n\n", iret );
		break;

	    case 12:	/* CRG_GGRP */

		printf("Enter the element number: \n");
	  	scanf(" %i", &elnum);

		crg_ggrp(elnum, &grptyp, &grpnum, &iret);
                printf ( "\nCRG_GGRP: iret = %d\n\n", iret );
		if ( iret == 0 ) { 
                    printf ( "group type = %d, group number = %d\n", 
							grptyp, grpnum );
		}
		break;

	    case 13:	/* CRG_GGNEL */

		printf("Enter the group type and group number: \n");
	  	scanf(" %i %i", &ityp, &grpnum);
		grptyp = ityp;

		crg_ggnel(grptyp, grpnum, &nelm, &iret);
                printf ( "\nCRG_GGNEL: iret = %d\n\n", iret );
		if ( iret == 0 ) { 
                    printf ( "total number of elements in this group = %d\n", 
							nelm );
		}
		break;

	    case 14:	/* CRG_GGINX */

		printf("Enter the group type and group number: \n");
	  	scanf(" %i %i", &ityp, &grpnum);
		grptyp = ityp;

		crg_ggnel(grptyp, grpnum, &nexp, &iret);
		inxarry = (int *)malloc(nexp*sizeof(int));
		crg_gginx(grptyp, grpnum, nexp, inxarry, &nelm, &iret);
                printf ( "\nCRG_GGNEL: iret = %d\n\n", iret );
		if ( iret == 0 ) { 
                    printf ( "The indecies of the elment in this group is:\n");
		    for (i = 0; i < nelm; i++) {
			printf("element %d\n", inxarry[i]);
		    }
		}
		free(inxarry);
		break;

	    case 15:	/* CRG_GGBND */

		printf("Enter the group type and group number: \n");
	  	scanf(" %i %i", &ityp, &grpnum);
		grptyp = ityp;

		crg_ggbnd(grptyp, grpnum, &rl, &rr, &rt, &rb, &iret);
                printf ( "\nCRG_GGBND: iret = %d\n\n", iret );
		if ( iret == 0 ) { 
                    printf ( "The bound of this group: rl = %.2f, rr = %.2f, rt =%.2f rb = %.2f \n", rl, rr, rt, rb); 
		}
		break;

	    case 16:	/* CRG_GGNXT */

		printf("Enter the group type: \n");
	  	scanf(" %i", &ityp);
		grptyp = ityp;

		crg_ggnxt(grptyp, &grpnum, &iret);
                printf ( "\nCRG_GGNXT: iret = %d\n\n", iret );
		if ( iret == 0 ) { 
                    printf ( "The next group number: %d\n", grpnum); 
		}
		break;

	    case 17:	/* CRG_STYP */

		printf("Enter the element number: \n");
	  	scanf(" %i", &elnum);

		printf("Enter the vg_class: \n");
	  	scanf(" %i", &ityp);
		vg_class = ityp;

		printf("Enter the vg_type: \n");
	  	scanf(" %i", &ityp);
		vg_type = ityp;

		crg_styp(elnum, vg_class, vg_type, &iret);
                printf ( "\nCRG_STYP: iret = %d\n\n", iret );
		break;

	    case 18:	/* CRG_GTYP */

		printf("Enter the element number: \n");
	  	scanf(" %i", &elnum);

		crg_gtyp(elnum, &vg_type, &vg_type, &iret);
                printf ( "\nCRG_GTYP: iret = %d\n\n", iret );
		if ( iret == 0 ) { 
                    printf ( "vg_class = %d, vg_type = %d\n", 
			    vg_class, vg_type );
		}
		break;

	    case 19:	/* CRG_SETLAYER */

		printf("Enter the element number: \n");
	  	scanf(" %i", &elnum);

		printf("Enter the layer: \n");
	  	scanf(" %i", &layer);

		crg_setLayer(elnum, layer, &iret);                
                printf ( "\nCRG_SETLAYER: iret = %d\n\n", iret );
		sprintf(logstr, "%i ", joffset);
		er_lmsg ( &level, grp, &iret, NULL, &ier, strlen(grp));
		break;

	    case 20:	/* CRG_GETLAYER */

	  	printf("Enter the file pos of the element to find layer\n");
                scanf ( " %i", &joffset );

		layer = crg_getLayer( joffset );
		if ( layer >= 0 ) {
		    printf("\nElement at file pos %i is at layer %i\n",
				joffset, layer);
		}
		else {
		    printf("\nElement is not found at offset %i\n", joffset);
		}
		break;

	    case 21:	/* CRG_CLEARLAYER */

		printf("Enter the layer you want to clear: \n");
	  	scanf(" %i", &layer);

		crg_clearLayer( layer, &iret );
                printf ( "\nCRG_CLEARLAYER: iret = %d\n\n", iret );
		break;

	    case 22:    /* CRG_MVALLAYER */
		printf ("Enter the layer to which all elements will be assigned:\n");
	  	scanf(" %i", &layer);

		crg_mvallayer (layer, &moved, &iret);
		printf ("	CRG_MVALLAYER: iret = %d\n\n", iret );
	        break;

            case 23:    /* CRG_GGNHL */

                printf("Enter the group type: \n");
                scanf(" %i", &ityp);
                grptyp = ityp;

                crg_ggnhl(grptyp, &high_grpnum, &low_grpnum, &iret);
                printf ( "\nCRG_GGNHL: iret = %d\n\n", iret );
                if ( iret == 0 ) {
                    printf ( "The largest group number: %d\n", high_grpnum);
		    printf ( "The smallest group number: %d\n", low_grpnum);
                }
                break;

            case 24:    /* CGR_GOFFSETS */
                if (initflg == 0) {
                    printf ("\n*****CRG_INIT MUST BE INVOKED FIRST*****\n");
                    break;
                }
                if (filnam[0] == '\0') {
                    printf ("\n*****OPEN VGF BEFORE LISTING RANGE RECORD\n");
                    break;
                }

		printf("Enter the vg_class: \n");
	  	scanf(" %i", &ityp);
		vg_class = ityp;

		printf("Enter the vg_type: \n");
	  	scanf(" %i", &ityp);
		vg_type = ityp;

		printf ("Enter the layer: \n");
	  	scanf(" %i", &layer);

                crg_goffsets( vg_class, vg_type, layer, offsets, &num_off );
		printf("  number of offsets returned = %d\n", num_off );

		if( num_off > 0 ) {
                    printf("  Offset List:\n" );
		    for ( ii=0; ii < num_off; ii++ ) {
		        printf("    offset[%d] = %d\n", ii, offsets[ii] );
                    }
                }
                break;

            case 25:    /* CGR_BUILD */

                printf ( "Enter the VG file name to open:\n" );
                scanf ( " %s", filnam );

                printf ("Enter the layer: \n");
                scanf(" %i", &layer);

                crg_build( filnam, &layer, &iret );
                printf ( "\nCRG_BUILD: iret = %d\n\n", iret );
                                                                                                                                    
                break;

	    case 26:

                printf ( "Enter the element number to set:\n" );
                scanf ( " %i", &elnum );

                printf ( "Enter the element number to be linked:\n" );
                scanf ( " %i", &assocrec );

		crg_sarec( elnum, assocrec, &iret );
		printf ( "\nCRG_sarec: iret = %d\n\n", iret );
		er_lmsg ( &level, grp, &iret, filnam, &ier, strlen(grp),
		  strlen(filnam) );
    		break;

	    case 27:

                printf ( "Enter the element number to get second record number:\n" );
                scanf ( " %i", &elnum );

		crg_gassocrec ( elnum, &assocrec, &iret);
		
		if ( iret == 0 ) {
		    printf ( "Record %d has a second record at %d\n",
		              elnum, assocrec );
		}
		
		printf ( "\nCRG_GASSOCREC: iret = %d\n\n", iret );
		er_lmsg ( &level, grp, &iret, filnam, &ier, strlen(grp),
		  strlen(filnam) );
    		break;

	    case 28:

                printf ( "Enter the element number to check:\n" );
                scanf ( " %i", &elnum );
		
		if ( crg_isauxrec ( elnum, &iret ) ) {
		    printf ( "\nRecord %d is a secondary record\n", elnum );	
		}
		else {
		    printf ( "\nRecord %d is a primary record\n", elnum );	
		}
		
		printf ( "\nCRG_isauxrec: iret = %d\n\n", iret );
		er_lmsg ( &level, grp, &iret, filnam, &ier, strlen(grp),
		  strlen(filnam) );
    		break;

	    case 29:

                printf ( "Enter the element number to set as second record:\n" );
                scanf ( " %i", &elnum );

		crg_setauxrec ( elnum, &iret );
		printf ( "\nCRG_SETAUXREC: iret = %d\n\n", iret );
		er_lmsg ( &level, grp, &iret, filnam, &ier, strlen(grp),
		  strlen(filnam) );
    		break;
	    
	    case 30:

                printf ( "Enter the element number to link attribute:\n" );
                scanf ( " %i", &elnum );

		crg_lkattr ( elnum, &iret );
		printf ( "\nCRG_LKATTR: iret = %d\n\n", iret );
		er_lmsg ( &level, grp, &iret, filnam, &ier, strlen(grp),
		  strlen(filnam) );
    		break;
		
	    case 31:

                printf ( "Enter the element number to get its second range box:\n" );
                scanf ( " %i", &elnum );

                if ( initflg == 0 ) {
                    crg_init( &ier );
                }

		crg_getsecrange ( elnum, &llx, &lly, &urx, &ury, &iret );
		printf ( "\nCRG_GETSECRANGE: iret = %d\n\n", iret );
		er_lmsg ( &level, grp, &iret, filnam, &ier, strlen(grp),
		  strlen(filnam) );
    		break;
	    
	    default:

		printf (" ******Invalid command\n");

	    }
	}
    }
    return(0);
}
