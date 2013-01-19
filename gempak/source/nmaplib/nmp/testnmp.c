#include "nmpcmn.h"
#include "Nxm.h"


static void dumpnmp ( int *iret );
static void dumpmap ( void );
static void dumpovl ( void );
static void dumpsavovl ( void );
static void dumpmaps ( void );
static void dumpsaved_map ( void );
static int  getin ( char *str, int min, int max );


int main ( void )
/************************************************************************
 * TESTNMP                                                              *
 *                                                                      *
 * This program test the NMP library of routines.                       *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC             9/00   Created				        *
 * M. Li/GSC		11/00	Added nmp_init, nmp_gmapnum, nmp_gmapnms*
 *				  and dumpnmp				*
 * M. Li/GSC		11/00	Added nmp_govlattr, nmp_govlflg, 	*
 *				  nmp_govlnms, nmp_govlnum, dumpmap and	*
 *			          dumpovl				*
 * M. Li/GSC		12/00	Added nmp_sovlattr, nmp_savovl, and	*
 *				  nmp_restorovl				*
 * M. Li/GSC		12/00	Added nmp_setmap, nmp_szoom, 		*
 *				  nmp_gmapattr, nmp_smapattr, nmp_svmap	*
 *				  and nmp_restormap			*
 * M. Li/GSC		01/01	Added checked for invalid input		*
 * M. Li/GSC		02/01	add nmp_gltln, nmp_mkstn,     		*
 *                                nmp_setmapstr,nmp_sproj, nmp_simf	*
 * E. Safford/GSC	05/01	add param to nmp_simf			*
 * E. Safford/GSC	05/01	add nmp_simmap                		*
 * M. Li/GSC		05/01	add nmp_save and nmp_restore		*
 * E. Safford/SAIC	08/01	fix set proj & plot & cleanup		*
 * H. Zeng/EAI          08/01   added nmp_sdefmap                       *
 * J. Wu/SAIC		08/01   add nmp_rdeflts                   	* 
 * H. Zeng/EAI          05/02   removed nmap_gmapstr                    *
 * T. Piper/SAIC	08/04	changed array sizes from 50 to MAX_OVL	*
 * T. Piper/SAIC	08/04	added nmp_mkscl				*
 * T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
 ************************************************************************/
{
        int     ii, cont, ier, iret, numsub, istat, mode, mapnum, ovlnum;
	int	type, idrpfl, lp, ovl, allp, flg, itype, imgtyp;
        char    	ergrp[4], erstr[81], select[80], proj[25], garea[128];

        char    	panel[81], device[81], map[21], attribs[30], temp[10];
	char    	filenam[256], imgfile[256];
	nmpstr_t	mapnms[MAX_OVL], ga[2], mapAdd;
	nmpovlstr_t	ovlnms[MAX_OVL], ovlattr;

	Boolean		ovlflg[MAX_OVL];	

/*---------------------------------------------------------------------*/

        in_bdta ( &ier );
	gd_init ( &ier );

        mode = 1;
        ginitp ( &mode, &istat, &ier );

        printf ( "Enter full DEVICE string:\n" );
        scanf ( " %s", device );

        gg_sdev ( device, &ier, strlen ( device ) );

        strcpy ( ergrp, "NMP" );

        cont = G_TRUE;

        while ( cont ) {
            printf ( "\n\n" );
            printf ( "   1 = NMP_INIT       2 = NMP_GMAPNUM     3 = NMP_GMAPNMS\n" );
	    printf ( "   4 = NMP_GOVLNUM    5 = NMP_GOVLNMS     6 = NMP_GOVLFLG\n" );
	    printf ( "   7 = NMP_GOVLATTR   8 = NMP_SOVLATTR    9 = NMP_SAVE\n" );
	    printf ( "  10 = NMP_RESTORE   11 = NMP_SETMAP     12 = NMP_SZOOM\n" );
	    printf ( "  13 = NMP_GMAPATTR  14 = NMP_SMAPATTR   15 = NMP_SDEFMAP\n" );
	    printf ( "  16 = NMP_DSPL      17 = NMP_SETMAPSTR  18 = NMP_GLTLN\n");
	    printf ( "  19 = NMP_MKSTN     20 = NMP_MKSCL      21 = NMP_SOVLFLG\n");
	    printf ( "  22 = NMP_SIMF      23 = NMP_SIMMAP     24 = NMP_VALID\n");
	    printf ( "  25 = NMP_RDEFLTS\n");
	    printf ( "  31 = set proj\n");
	    printf ( "  \n");
	    printf ( "  99 = DUMP\n"); 
            printf ( "\n" );
            printf ( "Select a subroutine number or type EXIT: " );
            scanf ( " %s", select );
            switch ( select[0] ) {
                case 'e':
                case 'E':
                        cont = G_FALSE;
                default:
                        cst_numb(select, &numsub, &ier);
                        break;
            }

/*---------------------------------------------------------------------*/
            if  ( numsub == 1 )  {	/* NMP_INIT	*/

		nmp_init(&iret);

		printf ( "iret = %d\n", iret );

		if (iret != 0) {
		    strcpy ( erstr, " " );
		    er_wmsg ( ergrp, &iret, erstr, &ier,
			      strlen ( ergrp ), strlen ( erstr ) );
		}
	    }
/*---------------------------------------------------------------------*/
	    if  ( numsub == 2 )  {	/* NMP_GMAPNUM	*/

		nmp_gmapnum(&mapnum, &iret);

		printf ( "iret = %d\n", iret );

                if (iret == 0) {
                    printf("Number of map areas = %d\n", mapnum);
                }
                else {
                    strcpy ( erstr, "select NMP_INIT and try again. " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
            }
/*---------------------------------------------------------------------*/
            if  ( numsub == 3 )  {	/* NMP_GMAPNMS	*/

                nmp_gmapnms(mapnms, &iret);

                printf ( "iret = %d\n", iret );

                if (iret == 0) {
		    for ( ii = 0; ii < num_maps; ii++ ) {
			printf (" Name of map area = %s\n", mapnms[ii]);
		    }
                }
                else {
                    strcpy ( erstr, "select NMP_INIT and try again." );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
            }
/*---------------------------------------------------------------------*/
            if  ( numsub == 4 )  {       /* NMP_GOVLNUM  */

                nmp_govlnum(&ovlnum, &iret);

                printf ( "iret = %d\n", iret );

                if (iret == 0) {
                    printf("Number of overlays = %d\n", ovlnum);
                }
                else {
                    strcpy ( erstr, "select NMP_INIT and try again." );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
            }
/*---------------------------------------------------------------------*/
            if  ( numsub == 5 )  {       /* NMP_GOVLNMS   */

                nmp_govlnms(ovlnms, &iret);

                printf ( "iret = %d\n", iret );

                if (iret == 0) {
                    for ( ii = 0; ii < overlay[0].novl; ii++ ) {
                        printf (" Name of overlay = %s\n", ovlnms[ii]);
                    }
                }
                else {
                    strcpy ( erstr, "Select NMP_INIT and try again " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
            }
/*---------------------------------------------------------------------*/
            if  ( numsub == 6 )  {       /* NMP_GOVLFLG   */
		
		lp = getin("Loop index", 0, MAX_LOOP-1);

                nmp_govlflg( lp, ovlflg, &iret);

                printf ( "iret = %d\n", iret );

                if (iret == 0) {
                    for ( ii = 0; ii < overlay[lp].novl; ii++ ) {
                        printf (" overlay flag setting = %d\n", ovlflg[ii]);
                    }
                }
                else {
                    strcpy ( erstr, "Select NMP_INIT and try again " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
            }
/*---------------------------------------------------------------------*/
            if  ( numsub == 7 )  {       /* NMP_GOVLATTR   */

                lp = getin("loop index", 0, MAX_LOOP-1);
                ovl = getin("overlay number", 0, 29);

                nmp_govlattr( ovl, lp, &itype, ovlattr, &iret);

                printf ( "iret = %d\n", iret );

                if (iret == 0) {
		    printf (" itype = %d\n", itype);
                    printf (" overlay attribute = %s\n", ovlattr);
                }
                else {
                    strcpy ( erstr, "Select NMP_INIT and try again " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
            }
/*---------------------------------------------------------------------*/
            if  ( numsub == 8 )  {       /* NMP_SOVLATTR   */

		lp = getin("loop index", 0, MAX_LOOP-1);
		ovl = getin("overlay number", 0, 29);
		type = getin("type", 0, 5);

		if (type == 0) {
		    cont = 5;
		} else if (type == 1) {
		    cont = 3;
		} else if (type == 2) {
		    cont = 6;
		} else if (type == 5) {
		    cont = 10;
	    }

		printf ("Enter overlay attribute string (e.g., 2 4 7, or 12 1 2 3 5):\n");
		strcpy (ovlattr, "\0");

		for (ii = 0; ii < cont; ii++ ) {
		    strcpy(temp, "\0");
		    scanf ("%s", temp);
		    if ( ii < cont - 1 ) strcat (temp, " ");
		    strcat (ovlattr, temp);
		} 	

                nmp_sovlattr( lp, ovl, ovlattr, &iret);

                printf ( "iret = %d\n", iret );

                if (iret == 0) {
                    printf (" new overlay attribute = %s\n", 
			overlay[lp].mapovl[ovl].attr);
                }
                else {
                    strcpy ( erstr, "Attribute string does not match overlay type. " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
            }
/*---------------------------------------------------------------------*/
            if  ( numsub == 9 )  {      /* NMP_SAVE       */
		nmp_save( &iret );
		printf (" End of NMP_SAVE ...\n");
            }
/*---------------------------------------------------------------------*/
            if  ( numsub == 10 )  {      /* NMP_RESTORE       */
                nmp_restore( &iret );
                printf (" End of NMP_RESTORE ...\n");
            }
/*---------------------------------------------------------------------*/
            if  ( numsub == 11 )  {       /* NMP_SETMAP     */

		lp = getin("loop index", 0, MAX_LOOP-1);
                printf ("Enter a GEMPAK map name (e.g., NCNA, check table mapinfo.tbl): \n");
                scanf ("%s", map );
		printf ("Apply to all loops ? (1 = yes, 0 = no) \n");
		scanf ("%d", &allp);

		if (num_maps > 0) {
		    for ( ii = 0; ii < num_maps; ii++ ) {
			if (strcmp(map, map_tbl[ii].geog) == 0 ) {
			    strcpy(map, map_tbl[ii].name);
			    break;
			}
	  	    }	
		    
		    nmp_setmap(map, lp, allp, &iret);

	 	    printf ( "iret = %d\n", iret );
                    if (iret != 0) {
                        strcpy ( erstr, "" );
                        er_wmsg ( ergrp, &iret, erstr, &ier,
                            strlen ( ergrp ), strlen ( erstr ) );
                    }

		}
		else {
		    printf("Select NMP_INIT and try again.\n");
		}
	    }
/*---------------------------------------------------------------------*/
            if  ( numsub == 12 )  {       /* NMP_SZOOM      */

		lp = getin("loop index", 0, MAX_LOOP-1);
                printf ("Enter zoom area (e.g., 30;-110;40;-100): \n");
                scanf ("%s", garea );

		nmp_szoom (lp, garea, &iret);

                if (iret == 0) {
                    printf (" iret = %d\n", iret);
                } else {
                    strcpy ( erstr, "" );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
            }
/*---------------------------------------------------------------------*/
            if  ( numsub == 13 )  {       /* NMP_GMAPATTR   */

		lp = getin("loop index", 0, MAX_LOOP-1);

                nmp_gmapattr (lp, mapAdd, proj, ga, &iret);

		printf ( "iret = %d\n", iret );

                if (iret == 0) {
                    printf ("          map = %s\n", mapAdd);
                    printf ("         proj = %s\n", proj);
		    printf ("        garea = %s\n", ga[0]);
		    printf (" zoomed garea = %s\n\n", ga[1]); 
                } else {
                    strcpy ( erstr, "" );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
            }
/*---------------------------------------------------------------------*/
            if  ( numsub == 14 )  {       /* NMP_SMAPATTR      */

		lp = getin("loop index", 0, MAX_LOOP-1);
		printf ("Enter map: (e.g., Alaska) \n");
		scanf ("%s", mapAdd );
		printf ("Enter proj: \n");
		scanf ("%s", proj );
		printf ("Enter unzoomed garea (e.g., DSET or 30;-110;40;-100):\n");
		scanf ("%s", ga[0]);
		printf ("Enter zoomed garea (e.g., 30;-110;40;-100):\n");
		scanf ("%s", ga[1]);
		printf ("Apply to all loops ? (1 = yes, 0 = no) \n");
		scanf ("%d", &allp);

		nmp_smapattr (lp, mapAdd, proj, ga, allp, &iret);

		printf ( "iret = %d\n", iret );

		if (iret == 0) {
		    printf ("          map = %s\n", maps[lp].map);
		    printf ("         proj = %s\n", maps[lp].proj);
		    printf ("        garea = %s\n", maps[lp].garea[0]);
		    printf (" zoomed garea = %s\n\n", maps[lp].garea[1]);
		} else {
		    strcpy ( erstr, "" );
		    er_wmsg ( ergrp, &iret, erstr, &ier,
			      strlen ( ergrp ), strlen ( erstr ) );
		}
	    }
/*---------------------------------------------------------------------*/

            if  ( numsub == 15 )  {       /* NMP_SDEFMAP  */

                lp  = getin("loop index", 0, MAX_LOOP-1);

                nmp_sdefmap( lp, &iret );

                printf ( "iret = %d\n", iret );

                if (iret != 0) {
                    strcpy ( erstr, " " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
	    }
/*---------------------------------------------------------------------*/

            if  ( numsub == 16 )  {     /* NMP_DSPL     */

                lp  = getin("Which loop settings should be plotted?", 
							0, MAX_LOOP-1);
		printf ( "Enter panel:\n" );
		scanf  ( " %s", panel );

		printf ( "Enter type: ( 0=lat/lon, 1=map, 2-4=stations, 5=scale)\n" );
		scanf  ( " %d", &type);

		if ( type == 0 )
		    strcpy (filenam, " ");
		else if ( type >= 1 && type <= 4 ) {
		    strcpy ( filenam, maps[lp].mapfile);
		}

		strcpy ( attribs, maps[lp].mapattr );

                nmp_dspl(panel, &type, filenam, attribs, &iret,
                        strlen(panel), strlen(filenam), strlen(attribs));
                printf ( "iret = %d\n", iret );

                if  ( iret != 0 )  {
                    strcpy ( erstr, " " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
		geplot (&iret);
            }
/*---------------------------------------------------------------------*/

            if  ( numsub == 17 )  {       /* NMP_SETMAPSTR   */

                lp = getin("loop index", 0, MAX_LOOP-1);
		
		nmp_setmapstr(lp, &iret);

                printf ( "iret = %d\n", iret );

                if (iret == 0) {
                    printf (" mapfile = %s\n", maps[lp].mapfile);
		    printf (" mapattr = %s\n", maps[lp].mapattr);
                }
                else {
                    strcpy ( erstr, "Select NMP_INIT and try again " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
           }
/*---------------------------------------------------------------------*/
           if  ( numsub == 18 )  {       /* NMP_GLTLN   */

                lp = getin("loop index", 0, MAX_LOOP-1);

                nmp_gltln(lp, ovlattr, &iret);

                printf ( "iret = %d\n", iret );

                if (iret == 0) {
                    printf (" ltln_str = %s\n", ovlattr);
                }
                else {
                    strcpy ( erstr, "Select NMP_INIT and try again " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
            }
/*---------------------------------------------------------------------*/
            if  ( numsub == 19 )  {       /* NMP_MKSTN   */

		lp  = getin("loop index", 0, MAX_LOOP-1);
                ovl = getin("overlay number", 0, 29);

                nmp_mkstn( lp, ovl, ovlattr, &iret);

                printf ( "iret = %d\n", iret );

                if (iret == 0) {
                    printf (" STN string = %s\n", ovlattr);
                }
                else {
                    strcpy ( erstr, "Select NMP_INIT and try again " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
            }
/*---------------------------------------------------------------------*/
            if  ( numsub == 20 )  {       /* NMP_MKSCL   */

		lp  = getin("loop index", 0, MAX_LOOP-1);
		ovl = getin("overlay number", 0, 29);

		nmp_mkscl( lp, ovl, ovlattr, &iret);

		printf ( "iret = %d\n", iret );

		if (iret == 0) {
		    printf (" STN string = %s\n", ovlattr);
		}
		else {
		    strcpy ( erstr, "Select NMP_INIT and try again " );
		    er_wmsg ( ergrp, &iret, erstr, &ier,
			      strlen ( ergrp ), strlen ( erstr ) );
		}
	    }
/*---------------------------------------------------------------------*/
            if  ( numsub == 21 )  {       /* NMP_SOVLFLG   */

                lp  = getin("loop index", 0, MAX_LOOP-1);
                ovl = getin("overlay number", 0, 29);
		flg = getin("Flag( 0 = FALSE, 1 = TRUE)", 0, 1);	

                nmp_sovlflg( lp, ovl, flg, &iret);

                printf ( "iret = %d\n", iret );

                if (iret == 0) {
                    printf (" flag = %d\n", overlay[lp].mapovl[ovl].active);
                }
                else {
                    strcpy ( erstr, "Select NMP_INIT and try again " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
            }
/*---------------------------------------------------------------------*/
            if  ( numsub == 22 )  {       /* NMP_SIMF   */

                lp  = getin("loop index", 0, MAX_LOOP-1);
		printf("Enter image file name:\n");
		scanf("%s", imgfile);

           	printf ("Enter 0 = No Image, 1 = SAT Image, 2 = RAD Image\n");
                scanf  ( " %d", &imgtyp);
		if ( imgtyp < NO_IMG || imgtyp > RAD_IMG) {
		    printf ("	Invalid image type, using %d\n", NO_IMG);
		    imgtyp = NO_IMG;
		}

                nmp_simf( lp, imgfile, imgtyp, &iret);

                printf ( "iret = %d\n", iret );

                if (iret == 0) {
                    printf (" image file name = %s\n", maps[lp].imgfile);
                }
                else {
                    strcpy ( erstr, "Select NMP_INIT and try again " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
            }
/*---------------------------------------------------------------------*/

            if  ( numsub == 23 )  {       /* NMP_SIMMAP   */

	        lp = getin("loop index", 0, MAX_LOOP-1);
	        printf ("Enter image type -- 1 = SAT, 2 = RAD\n");
	        scanf ("%d", &imgtyp);
	
	        printf ("Apply all loops?\n");
	        scanf ("%d", &allp);

	        nmp_simmap (imgtyp, lp, allp, &iret);

                if (iret == 0) {
                    printf (" imgtyp = %d\n", imgtyp);
		    if (allp) {
		        printf ("	set for loop all loops\n");
		    }
		    else {
		        printf ("	set for loop %d\n", lp);
		    }
                }
                else {
                    strcpy ( erstr, "Select NMP_SIMMAP and try again " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
	    }

/*---------------------------------------------------------------------*/

            if  ( numsub == 24 )  {       /* NMP_VALID   */

	        lp = getin("loop index", 0, MAX_LOOP-1);
		printf ("Enter map: (e.g., Alaska) \n");
                scanf ("%s", mapAdd );
                printf ("Enter proj: \n");
                scanf ("%s", proj );
		printf ("Enter unzoomed garea (e.g., DSET or 30;-110;40;-100):\n");
		scanf ("%s", ga[0]);
		printf ("Enter zoomed garea (e.g., 30;-110;40;-100):\n");
		scanf ("%s", ga[1]);
		nmp_valid (lp, mapAdd, ga, proj, &iret);
		
		printf ("	iret = %d\n", iret);
		if (iret < 0) {
		    erstr[0] = '\0';
                    er_wmsg ( ergrp, &iret, erstr, &ier, 
		    			strlen(ergrp), strlen(erstr) );
		}
	    }

/*---------------------------------------------------------------------*/

            if  ( numsub == 25 )  {      /* NMP_RDEFLTS */
                nmp_rdeflts( &iret );
                
		printf (" End of NMP_RDEFLTS iret = %d\n", iret );
		
		if (iret != 0) {
		    erstr[0] = '\0';
                    er_wmsg ( ergrp, &iret, erstr, &ier, 
		    			strlen(ergrp), strlen(erstr) );
		}
            }

/*---------------------------------------------------------------------*/

            if  ( numsub == 31 )  {	/* Set projection	*/
	        lp = getin("loop index", 0, MAX_LOOP-1);

		strcpy ( proj, maps[lp].proj );
		strcpy ( garea, maps[lp].garea[0] );

		gg_maps (proj, garea, " ", &idrpfl, &ier, 
					strlen(proj), strlen(garea), 1);
		printf ("	gg_maps called, ier = %d\n", ier);

                if  ( ier != 0 )  {
                    strcpy ( erstr, " " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
            }

/*---------------------------------------------------------------------*/

            if  ( numsub == 99 )  {	/* DUMP		*/
                dumpnmp ( &iret );
            }

/*---------------------------------------------------------------------*/
	}

	return 0;
}

/*=====================================================================*/

static void dumpnmp ( int *iret )
/************************************************************************
 * dumpnmp                                                              *
 *                                                                      *
 * This routine dumps the contents of the data structures in NMP 	*
 * library								*
 *									*
 * static void dumpnmp ( iret )                                         *
 *									*
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *									*
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		11/00	Created					*
 * M. Li/GSC		12/00	Added dumpsavovl			*
 * M. Li/GSC		12/00	Added dumpmaps, and dumpsaved_map	*
 ***********************************************************************/
{

int	i;

/*---------------------------------------------------------------------*/
    *iret = 0;

    printf (" 1 = dump map_tbl   2 = dump overlay  3 = dump saved_overlay\n");
    printf (" 4 = dump maps      5 = dump saved_map\n");
    printf ("10 = dump all    \n");
    scanf("%d", &i);

    switch (i) {
	case 1:		/* Dump map_tbl	*/
   	    dumpmap(); 
	    break;

	case 2:		/* Dump overlay */
	    dumpovl();
	    break;

        case 3:         /* Dump saved_overlay */
            dumpsavovl();
            break;

	case 4:		/* Dump maps	      */
	    dumpmaps();
	    break;

	case 5:		/* Dump saved_map	*/
	    dumpsaved_map();
	    break;

	case 10:	/* Dump all	*/
  	    dumpmap();
	    dumpovl(); 
	    dumpsavovl();
	    dumpmaps();
	    dumpsaved_map();
            break;
    }

}

/*=====================================================================*/

static void dumpmap ( void )
/************************************************************************
 * dumpmap                                                              *
 *                                                                      *
 * This routine dumps the contents of the structure for the predefined	*
 * map areas. 								*
 *                                                                      *
 * static void dumpmap ()	                                        *
 *                                                                      *
 * Input parameters:							*
 * Output parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            11/00   Created                                 *
 ***********************************************************************/
{

int     ii;

/*---------------------------------------------------------------------*/

    printf("\n************ Dumping map structure ***************\n");

    if (num_maps <= 0) {
        printf ("Select NMP_INIT and try again.\n");
        return;
    }

    printf("Number of predefined map areas = %d\n", num_maps);
    for ( ii = 0; ii < num_maps; ii++ ) {
         printf ( " Name of map area = %-20s Abreviated garea name = %-10s\n",
               map_tbl[ii].name, map_tbl[ii].geog );
    }
}

/*=====================================================================*/

static void dumpovl ( void )
/************************************************************************
 * dumpovl                                                              *
 *                                                                      *
 * This routine dumps the contents of the structure for map overlay	*
 *                                                                      *
 * static void dumpovl ()                                               *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            11/00   Created                                 *
 ***********************************************************************/
{

int     ii, lp;

/*---------------------------------------------------------------------*/

    printf("\n************ Dumping overlay structure ***************\n");

    if (overlay[0].novl <= 0 ) {
	printf ("Select NMP_INIT and try again.\n");
	return;
    }

    for (lp=0; lp < MAX_LOOP; lp++) {
        printf("\nLoop = %d\n\n", lp );
	printf("              Number of overlay = %d\n", overlay[lp].novl);
	printf("===========================================================================\n");
	printf("Overlay#  Title            itype  gempak name      Active  attribute string\n");
	printf("---------------------------------------------------------------------------\n");
	
	for ( ii = 0; ii < overlay[lp].novl; ii++ ) {
	    printf("%4d     %-15s %5d    %-15s %5d    %-20s\n", ii, 
		overlay[lp].mapovl[ii].title,
		overlay[lp].mapovl[ii].ityp, overlay[lp].mapovl[ii].gname,
		overlay[lp].mapovl[ii].active, overlay[lp].mapovl[ii].attr);
	}
    }

}

/*=====================================================================*/

static void dumpsavovl ( void )
/************************************************************************
 * dumpsavovl                                                           *
 *                                                                      *
 * This routine dumps the contents of the structure for                 *
 * saved_overlay                                                        *
 *                                                                      *
 * static void dumpsavovl ()                                            *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            12/00   Created                                 *
 ***********************************************************************/
{

int     ii, lp;

/*---------------------------------------------------------------------*/

    printf("\n************ Dumping saved_overlay structure ***************\n");

    if (saved_overlay[0].novl <= 0 ) {
        printf ("Select NMP_INIT and try again.\n");
        return;
    }

    for (lp=0; lp<MAX_LOOP; lp++) {
        printf("\nLoop = %d\n\n", lp );
        printf("              Number of saved_overlay = %d\n", saved_overlay[lp].novl);
        printf("===========================================================================\n");
        printf("Overlay#  Title            itype  gempak name      Active  attribute string\n");
        printf("---------------------------------------------------------------------------\n");

        for ( ii = 0; ii < saved_overlay[lp].novl; ii++ ) {
            printf("%4d     %-15s %5d    %-15s %5d    %-20s\n", ii,
                saved_overlay[lp].mapovl[ii].title,
                saved_overlay[lp].mapovl[ii].ityp, saved_overlay[lp].mapovl[ii].gname,
                saved_overlay[lp].mapovl[ii].active, saved_overlay[lp].mapovl[ii].attr);
        }
    }

}

/*=====================================================================*/

static void dumpmaps ( void )
/************************************************************************
 * dumpmaps                                                           	*
 *                                                                      *
 * This routine dumps the contents of the structure for maps            *
 *                                                                      *
 * static void dumpmaps ()                                              *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            12/00   Created                                 *
 * M. Li/GSC		03/01   added imgfile, mapfile, mapattr and map *
 * E. Safford/GSC	05/01	added imgtyp to output			*
 ***********************************************************************/
{

int     lp;

/*---------------------------------------------------------------------*/

    printf("\n************ Dumping maps structure ***************\n");

    for (lp=0; lp<MAX_LOOP; lp++) {
        printf("\nLoop = %d\n", lp );
        printf("------------------------------------------------------------\n");
	printf("    image file     = %s\n", maps[lp].imgfile);
	printf("    image type     = %d\n", maps[lp].imgtyp);
	printf("    mapfile        = %s\n", maps[lp].mapfile);
	printf("    mapattr        = %s\n", maps[lp].mapattr);
	printf("    map            = %s\n", maps[lp].map);
	printf("    map projection = %s\n", maps[lp].proj);
	printf("    starting garea = %s\n", maps[lp].garea[0]);
	printf("    zoomed garea   = %s\n", maps[lp].garea[1]);
	printf("    graphics mode  = %d\n", maps[lp].mode);
    }

}

/*=====================================================================*/

static void dumpsaved_map ( void )
/************************************************************************
 * dumpsaved_map                                                        *
 *                                                                      *
 * This routine dumps the contents of the structure for                 *
 * saved_map.                                                           *
 *                                                                      *
 * static void dumpsaved_map ()                                         *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            12/00   Created                                 *
 * M. Li/GSC            03/01   added imgfile, mapfile, mapattr and map *
 * E. Safford/GSC	05/01	added imgtyp to output			*
 ***********************************************************************/
{

int     lp;

/*---------------------------------------------------------------------*/

    printf("\n************ Dumping saved_map structure ***************\n");

    for (lp=0; lp<MAX_LOOP; lp++) {
        printf("\nLoop = %d\n", lp );
        printf("------------------------------------------------------------\n");
        printf("    image file     = %s\n", saved_map[lp].imgfile);
	printf("    image type     = %d\n", maps[lp].imgtyp);
        printf("    mapfile        = %s\n", saved_map[lp].mapfile);
        printf("    mapattr        = %s\n", saved_map[lp].mapattr);
        printf("    map            = %s\n", saved_map[lp].map);
        printf("    map projection = %s\n", saved_map[lp].proj);
        printf("    starting garea = %s\n", saved_map[lp].garea[0]);
        printf("    zoomed garea   = %s\n", saved_map[lp].garea[1]);
        printf("    graphics mode  = %d\n", saved_map[lp].mode);
    }

}

/*=====================================================================*/

static int getin ( char *str, int min, int max )
/************************************************************************
 * getin                                                                *
 *                                                                      *
 * This function retrieves integer input from the keyboard.             *
 *                                                                      *
 * static int getin (str, min, max)                                    *
 * Input parameters:                                                    *
 *      *str            char    input integer string                    *
 *      min             int     lower bound of the expected integer     *
 *      max             int     upper bound of the expected integer     *
 * Output parameters:                                                   *
 *      getin           static int     return integer value             *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            01/01                                           *
 * T. Piper/SAIC	05/04	Changed type of ii from int to size_t	*
 * ***********************************************************************/
{
int     temp, ier;
char    inp[256];
size_t	ii;
static	char valid, in;
/*---------------------------------------------------------------------*/

    valid = 0;

    while (!valid) {
        in = 1;
        printf("Enter %s (%d ~ %d):", str, min, max);
        scanf("%s", inp);
        if (inp[strlen(inp)-1] == '\n') inp[strlen(inp)-1] = '\0';

        /*
         * check for invalid input
         */
        for (ii=0;ii<strlen(inp);ii++) {
            if (! ( isdigit(inp[ii]) || (ii == 0 && inp[0] == '-') ) ) {
                in = 0;
            }
        }

        /*
         * check for out of range
         */
        if (!in) {
            printf("invalid input, try again\n");
        }
        else {
            cst_numb(inp, &temp, &ier);
            if (temp < min || temp > max) {
                in = 0;
                printf("input out of range, try again\n");
            }
        }

        if (in) valid = 1;
    }

    return temp;

}

/*=====================================================================*/
